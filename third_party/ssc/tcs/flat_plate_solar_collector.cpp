/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include <stdexcept>
#include "flat_plate_solar_collector.h"
#include "lib_irradproc.h"

const double kPi = 3.1415926535897;


FlatPlateCollector::FlatPlateCollector()
{
    FRta_ = FRUL_ = iam_ = area_coll_ = heat_capacity_rate_test_ = m_dot_test_ = std::numeric_limits<double>::quiet_NaN();
}

FlatPlateCollector::FlatPlateCollector(const CollectorTestSpecifications &collector_test_specifications)
    :
    FRta_(collector_test_specifications.FRta),
    FRUL_(collector_test_specifications.FRUL),
    iam_(collector_test_specifications.iam),
    area_coll_(collector_test_specifications.area_coll),
    heat_capacity_rate_test_(collector_test_specifications.heat_capacity * collector_test_specifications.m_dot),
    m_dot_test_(collector_test_specifications.m_dot)
{

}

const double FlatPlateCollector::RatedPowerGain()   // [W]
{
    // Calculation taken from UI equation

    double G_T = 1.e3;                  // normal incident irradiance [W/m2]
    double T_inlet_minus_T_amb = 30.;   // [K]

    return area_coll_ * (FRta_*G_T - FRUL_*T_inlet_minus_T_amb);
}

const double FlatPlateCollector::UsefulPowerGain(const TimeAndPosition &time_and_position, const ExternalConditions &external_conditions)  // [W]
{
    Weather weather(external_conditions.weather);
    double ambient_temp(external_conditions.weather.ambient_temp);
    InletFluidFlow inlet_fluid_flow(external_conditions.inlet_fluid_flow);
    double albedo(external_conditions.albedo);

    PoaIrradianceComponents poa_irradiance_components = IncidentIrradiance(time_and_position, weather, albedo);
    double transmitted_irradiance = TransmittedIrradiance(time_and_position.collector_orientation, poa_irradiance_components);
    double absorbed_radiant_power = AbsorbedRadiantPower(transmitted_irradiance, inlet_fluid_flow, ambient_temp);
    double thermal_power_loss = ThermalPowerLoss(inlet_fluid_flow, ambient_temp);
    double useful_power_gain = absorbed_radiant_power - thermal_power_loss;

    return useful_power_gain;
}

const double FlatPlateCollector::T_out(const TimeAndPosition &time_and_position, const ExternalConditions &external_conditions)     // [C]
{
    double useful_power_gain = UsefulPowerGain(time_and_position, external_conditions);

    double m_dot = external_conditions.inlet_fluid_flow.m_dot;
    double specific_heat = external_conditions.inlet_fluid_flow.specific_heat;
    double mdotCp_use = m_dot * specific_heat * 1.e3; // mass flow rate (kg/s) * Cp_fluid (kJ/kg.K) * 1000 J/kJ
    double dT_collector = useful_power_gain / mdotCp_use;

    double T_in = external_conditions.inlet_fluid_flow.temp;
    return dT_collector + T_in;
}

const double FlatPlateCollector::area_coll()    // [m2]
{
    return area_coll_;
}

void FlatPlateCollector::area_coll(double collector_area /*m2*/)
{
    area_coll_ = collector_area;
}

const CollectorTestSpecifications FlatPlateCollector::TestSpecifications()
{
    CollectorTestSpecifications collector_test_specifications;
    collector_test_specifications.FRta = FRta_;
    collector_test_specifications.FRUL = FRUL_;
    collector_test_specifications.iam = iam_;
    collector_test_specifications.area_coll = area_coll_;
    collector_test_specifications.m_dot = m_dot_test_;
    collector_test_specifications.heat_capacity = heat_capacity_rate_test_ / m_dot_test_;

    return collector_test_specifications;
}

const PoaIrradianceComponents FlatPlateCollector::IncidentIrradiance(const TimeAndPosition &time_and_position,
    const Weather &weather,
    double albedo  /*-*/)   // [W/m2]
{
    double dni = weather.dni;
    double dhi = weather.dhi;
    double ghi = weather.ghi;

    int irrad_mode;
    irrad tt;
    if (std::isfinite(dni) && std::isfinite(dhi)) {
        irrad_mode = 0;     // 0 = beam & diffuse
        tt.set_beam_diffuse(dni, dhi);
    }
    else if (std::isfinite(ghi) && std::isfinite(dni)) {
        irrad_mode = 1;     // 1 = total & beam
        tt.set_global_beam(ghi, dni);
    }
    else if (std::isfinite(ghi) && std::isfinite(dhi)) {
        irrad_mode = 2;     // 2 = total & diffuse
        tt.set_global_diffuse(ghi, dhi);
    }
    else {
        throw std::invalid_argument("FlatPlateCollector: Two of the three irradiance components must be specified.");
    }

    tt.set_location(time_and_position.collector_location.latitude,
        time_and_position.collector_location.longitude,
        time_and_position.collector_location.timezone);
    tt.set_optional(0, 1013.25, weather.ambient_temp);

    //double ts_hour = 1.0 / step_per_hour;
    //double delt = instantaneous ? IRRADPROC_NO_INTERPOLATE_SUNRISE_SUNSET : ts_hour;
    double irradproc_no_interpolate_sunrise_sunset = -1.0;      // IRRADPROC_NO_INTERPOLATE_SUNRISE_SUNSET = -1.0;
    double delt = irradproc_no_interpolate_sunrise_sunset;      // 
    tt.set_time(time_and_position.timestamp.tm_year + 1900,     // years since 1900
        time_and_position.timestamp.tm_mon + 1,                 // Jan. = 0
        time_and_position.timestamp.tm_mday,
        time_and_position.timestamp.tm_hour,
        time_and_position.timestamp.tm_min,
        delt);
    int sky_model = 2;      // isotropic=0, hdkr=1, perez=2
    tt.set_sky_model(sky_model, albedo);
    double tilt = time_and_position.collector_orientation.tilt;
    double azimuth = time_and_position.collector_orientation.azimuth;
    tt.set_surface(0, tilt, azimuth, 0, 0, 0, false, 0.0);
    tt.calc();

    double poa_beam, poa_sky_diffuse, poa_ground_reflected_diffuse;
    tt.get_poa(&poa_beam, &poa_sky_diffuse, &poa_ground_reflected_diffuse, 0, 0, 0);
    //double I_incident = (ssc_number_t)(poa_beam + poa_sky_diffuse + poa_ground_reflected_diffuse); // total PoA on surface

    //double solalt, solazi;
    //tt.get_sun(&solazi, 0, &solalt, 0, 0, 0, 0, 0, 0, 0);

    double poa_beam_aoi = 0;
    tt.get_angles(&poa_beam_aoi, 0, 0, 0, 0); // note: angles returned in degrees

    PoaIrradianceComponents poa_irradiance_components;
    poa_irradiance_components.beam_with_aoi.at(0) = poa_beam;
    poa_irradiance_components.beam_with_aoi.at(1) = poa_beam_aoi;
    poa_irradiance_components.sky_diffuse_with_aoi.at(0) = poa_sky_diffuse;
    poa_irradiance_components.sky_diffuse_with_aoi.at(1) = std::numeric_limits<double>::quiet_NaN();
    poa_irradiance_components.ground_reflected_diffuse_with_aoi.at(0) = poa_ground_reflected_diffuse;
    poa_irradiance_components.ground_reflected_diffuse_with_aoi.at(1) = std::numeric_limits<double>::quiet_NaN();

    return poa_irradiance_components;
};



const double FlatPlateCollector::TransmittedIrradiance(const CollectorOrientation &collector_orientation,
    const PoaIrradianceComponents &poa_irradiance_components)   // [W/m2]
{
    // calculate transmittance through cover
    double Kta_d = 0.0;
    double Kta_b = 0.0;
    double Kta_g = 0.0;

    // incidence angle modifier (IAM) for beam (D&B eqn 6.17.10 pp 297)
    double aoi_beam = poa_irradiance_components.beam_with_aoi.at(1);
    if (aoi_beam <= 60.0) {
        Kta_b = 1 - iam_ * (1 / cos(aoi_beam*kPi / 180) - 1);
    }
    else if (aoi_beam > 60.0 && aoi_beam <= 90.0) {
        Kta_b = (1 - iam_)*(aoi_beam - 90.0)*kPi / 180;
    }
    if (Kta_b < 0) Kta_b = 0;

    double tilt = collector_orientation.tilt;
    // effective incidence angle for sky diffuse radiation (D&B eqn 5.4.2 pp 215)
    double theta_eff_diffuse = 59.7*kPi / 180 - 0.1388*tilt*kPi / 180 + 0.001497*tilt*kPi / 180 * tilt*kPi / 180;
    double cos_theta_eff_diffuse = cos(theta_eff_diffuse);

    // incidence angle modifier (IAM) for diffuse (D&B eqn 6.17.10 pp 297)
    if (theta_eff_diffuse <= kPi / 3.) {
        Kta_d = 1 - iam_ * (1 / cos_theta_eff_diffuse - 1);
    }
    else if (theta_eff_diffuse > kPi / 3. && theta_eff_diffuse <= kPi / .2) {
        Kta_d = (1 - iam_)*(theta_eff_diffuse - kPi / 2.);
    }
    if (Kta_d < 0) {
        Kta_d = 0;
    }

    // effective incidence angle modifier for ground reflected radiation (D&B eqn 5.4.1 pp 215)
    double theta_eff_ground = 90 * kPi / 180 - 0.5788*tilt*kPi / 180 + 0.002693*tilt*kPi / 180 * tilt*kPi / 180;
    double cos_theta_eff_ground = cos(theta_eff_ground);

    // incidence angle modifier (IAM) for ground reflected radiation (D&B eqn 6.17.10 pp 297)
    if (theta_eff_ground <= kPi / 3) {
        Kta_g = 1 - iam_ * (1 / cos_theta_eff_ground - 1);
    }
    else if (theta_eff_ground > kPi / 3 && theta_eff_ground <= kPi / 2) {
        Kta_g = (1 - iam_)*(theta_eff_ground - kPi / 2.);
    }
    if (Kta_g < 0) {
        Kta_g = 0;
    }

    double beam_shading_factor = 1.0;
    double diffuse_shading_factor = 1.0;
    // TODO - How are shading losses calculated? Why does their setup require a cmod argument? Shading currently ignored.
    //if (shad.fbeam(hour, solalt, solazi, jj, step_per_hour)) {
    //    beam_loss_factor = shad.beam_shade_factor();
    //}
    //diffuse_shading_factor = shad.fdiff();

    // TODO - Why are shading loss factors applied here and not at the incidence irradiance calculation?
    double poa_beam = poa_irradiance_components.beam_with_aoi.at(0);
    double poa_sky_diffuse = poa_irradiance_components.sky_diffuse_with_aoi.at(0);
    double poa_ground_reflected_diffuse = poa_irradiance_components.ground_reflected_diffuse_with_aoi.at(0);
    double I_transmitted =
        Kta_b * poa_beam * beam_shading_factor +
        Kta_d * poa_sky_diffuse * diffuse_shading_factor +
        Kta_g * poa_ground_reflected_diffuse;

    return I_transmitted;
}

const double FlatPlateCollector::AbsorbedRadiantPower(double transmitted_irradiance /*W/m2*/, const  InletFluidFlow &inlet_fluid_flow, double T_amb /*C*/)    // [W]
{
    double m_dot = inlet_fluid_flow.m_dot;
    double specific_heat = inlet_fluid_flow.specific_heat;
    double mdotCp_use = m_dot * specific_heat * 1.e3; // mass flow rate (kg/s) * Cp_fluid (kJ/kg.K) * 1000 J/kJ

    /* Flow rate corrections to FRta, FRUL (D&B pp 307) */
    double FprimeUL = -heat_capacity_rate_test_ * 1.e3 / area_coll_ * ::log(1 - FRUL_ * area_coll_ / (heat_capacity_rate_test_ * 1.e3) ); // D&B eqn 6.20.4
    double r = (mdotCp_use / area_coll_ * (1 - exp(-area_coll_ * FprimeUL / mdotCp_use))) / FRUL_; // D&B eqn 6.20.3
    double FRta_use = FRta_ * r; // FRta_use = value for this time step 

    double Q_dot_absorbed = area_coll_ * FRta_use*transmitted_irradiance; // from D&B eqn 6.8.1
    return Q_dot_absorbed;
}

const double FlatPlateCollector::ThermalPowerLoss(const InletFluidFlow &inlet_fluid_flow, double T_amb /*C*/)  // [W]
{
    double T_in = inlet_fluid_flow.temp;
    double m_dot = inlet_fluid_flow.m_dot;
    double specific_heat = inlet_fluid_flow.specific_heat;
    double mdotCp_use = m_dot * specific_heat * 1.e3; // mass flow rate (kg/s) * Cp_fluid (J/kg.K) * 1000 J/kJ

    double FprimeUL = -heat_capacity_rate_test_ * 1.e3 / area_coll_ * ::log(1 - FRUL_ * area_coll_ / (heat_capacity_rate_test_ * 1.e3) ); // D&B eqn 6.20.4
    double r = (mdotCp_use / area_coll_ * (1 - exp(-area_coll_ * FprimeUL / mdotCp_use))) / FRUL_; // D&B eqn 6.20.3

    double FRUL_use = FRUL_ * r; // FRUL_use = value for this time step
    double Q_dot_losses = area_coll_ * FRUL_use * (T_in - T_amb); // from D&B eqn 6.8.1
    return Q_dot_losses;
}



Pipe::Pipe()
{
    pipe_diam_ = pipe_k_ = pipe_insul_ = pipe_length_ = std::numeric_limits<double>::quiet_NaN();
}

Pipe::Pipe(double pipe_diam /*m*/, double pipe_k /*W/m-K*/, double pipe_insul /*m*/, double pipe_length /*m*/)
    : pipe_diam_(pipe_diam),
    pipe_k_(pipe_k),
    pipe_insul_(pipe_insul),
    pipe_length_(pipe_length)
{}

const double Pipe::pipe_od()    // [m]
{
    return pipe_diam_ + pipe_insul_ * 2;
}

const double Pipe::UA_pipe()    // [W/K]
{
    double U_pipe = 2 * pipe_k_ / (pipe_od() * ::log(pipe_od() / pipe_diam_)); //  **TODO** CHECK whether should be pipe_diam*log(pipe_od/pipe_diam) in denominator
    double UA_pipe = U_pipe * kPi * pipe_od() * pipe_length_; // W/'C
    return UA_pipe;
}

const double Pipe::ThermalPowerLoss(double T_in /*C*/, double T_amb /*C*/)   // [W]
{
    return UA_pipe()*(T_in - T_amb);
}

const double Pipe::T_out(double T_in /*C*/, double T_amb /*C*/, double heat_capacity_rate /*kW/K*/)     // [C]
{
    double thermal_power_loss = ThermalPowerLoss(T_in, T_amb);
    double T_out = -thermal_power_loss / (heat_capacity_rate * 1.e3) + T_in;
    return T_out;
}



FlatPlateArray::FlatPlateArray()
{
    flat_plate_collector_ = FlatPlateCollector();
    collector_location_ = CollectorLocation();
    collector_orientation_ = CollectorOrientation();
    array_dimensions_ = ArrayDimensions();
    inlet_pipe_ = outlet_pipe_ = Pipe();
}

FlatPlateArray::FlatPlateArray(const FlatPlateCollector &flat_plate_collector, const CollectorLocation &collector_location,
    const CollectorOrientation &collector_orientation, const ArrayDimensions &array_dimensions,
    const Pipe &inlet_pipe, const Pipe &outlet_pipe)
    :
    flat_plate_collector_(flat_plate_collector),
    collector_location_(collector_location),
    collector_orientation_(collector_orientation),
    array_dimensions_(array_dimensions),
    inlet_pipe_(inlet_pipe),
    outlet_pipe_(outlet_pipe)
{

}

FlatPlateArray::FlatPlateArray(const CollectorTestSpecifications &collector_test_specifications, const CollectorLocation &collector_location,
    const CollectorOrientation &collector_orientation, const ArrayDimensions &array_dimensions,
    const Pipe &inlet_pipe, const Pipe &outlet_pipe)
    :
    flat_plate_collector_(collector_test_specifications),
    collector_location_(collector_location),
    collector_orientation_(collector_orientation),
    array_dimensions_(array_dimensions),
    inlet_pipe_(inlet_pipe),
    outlet_pipe_(outlet_pipe)
{

}

const int FlatPlateArray::ncoll()
{
    return array_dimensions_.num_in_series * array_dimensions_.num_in_parallel;
}

const double FlatPlateArray::area_total()
{
    return flat_plate_collector_.area_coll() * ncoll();
}

void FlatPlateArray::resize_array(ArrayDimensions array_dimensions)
{
    if (array_dimensions.num_in_series <= 0 || array_dimensions.num_in_parallel <= 0) return;

    array_dimensions_ = array_dimensions;
}

void FlatPlateArray::resize_array(double m_dot_array_design /*kg/s*/, double specific_heat /*kJ/kg-K*/, double temp_rise_array_design /*K*/)
{
    if (!std::isnormal(m_dot_array_design) || !std::isnormal(specific_heat) || !std::isnormal(temp_rise_array_design)) return;
    if (m_dot_array_design <= 0. || specific_heat <= 0. || temp_rise_array_design <= 0.) return;
    
    CollectorTestSpecifications collector_test_specifications = flat_plate_collector_.TestSpecifications();

    // Number in parallel
    double m_dot_design_single_collector = collector_test_specifications.m_dot;
    double exact_fractional_collectors_in_parallel = m_dot_array_design / m_dot_design_single_collector;
    if (exact_fractional_collectors_in_parallel < 1.) {
        array_dimensions_.num_in_parallel = 1;
    }
    else {
        array_dimensions_.num_in_parallel = static_cast<int>(std::round(exact_fractional_collectors_in_parallel));      // std::round() rounds up at halfway point
    }
    double m_dot_series_string = m_dot_array_design / array_dimensions_.num_in_parallel;      // [kg/s]

    // Number in series
    double collector_rated_power = flat_plate_collector_.RatedPowerGain();  // [W]
    double collector_rated_temp_rise = collector_rated_power / (m_dot_series_string * specific_heat * 1.e3);
    double exact_fractional_collectors_in_series = temp_rise_array_design / collector_rated_temp_rise;
    if (exact_fractional_collectors_in_series < 1.) {
        array_dimensions_.num_in_series = 1;
    }
    else {
        array_dimensions_.num_in_series = static_cast<int>(std::round(exact_fractional_collectors_in_series));      // std::round() rounds up at halfway point
    }
}

const double FlatPlateArray::UsefulPowerGain(const tm &timestamp, const ExternalConditions &external_conditions)      // [W]
{
    TimeAndPosition time_and_position;
    time_and_position.collector_location = collector_location_;
    time_and_position.collector_orientation = collector_orientation_;
    time_and_position.timestamp = timestamp;
    double T_in = external_conditions.inlet_fluid_flow.temp;
    double T_amb = external_conditions.weather.ambient_temp;
    double m_dot = external_conditions.inlet_fluid_flow.m_dot;
    double specific_heat = external_conditions.inlet_fluid_flow.specific_heat;
    double specific_heat_capacity = m_dot * specific_heat;

    // Inlet pipe
    double inlet_pipe_thermal_power_loss = inlet_pipe_.ThermalPowerLoss(T_in, T_amb);
    double T_out_inlet_pipe = inlet_pipe_.T_out(T_in, T_amb, specific_heat_capacity);
    double T_array_in = T_out_inlet_pipe;

    // Collectors
    ExternalConditions external_conditions_to_collector(external_conditions);
    external_conditions_to_collector.inlet_fluid_flow.temp = T_array_in;
    external_conditions_to_collector.inlet_fluid_flow.m_dot = m_dot / array_dimensions_.num_in_parallel;
    double series_string_thermal_power_gain = 0.;
    double T_array_out = T_array_in;
    for (std::size_t i = 0; i < array_dimensions_.num_in_series; i++) {
        series_string_thermal_power_gain += flat_plate_collector_.UsefulPowerGain(time_and_position, external_conditions_to_collector);
        double T_collector_out = flat_plate_collector_.T_out(time_and_position, external_conditions_to_collector);
        external_conditions_to_collector.inlet_fluid_flow.temp = T_collector_out;   // to next collector in series
        T_array_out = T_collector_out;
    }

    // Outlet pipe
    double outlet_pipe_thermal_power_loss = outlet_pipe_.ThermalPowerLoss(T_array_out, T_amb);

    double useful_power_gain = -inlet_pipe_thermal_power_loss
        + series_string_thermal_power_gain * array_dimensions_.num_in_parallel
        - outlet_pipe_thermal_power_loss;
    return useful_power_gain;
}

const double FlatPlateArray::T_out(const tm &timestamp, const ExternalConditions &external_conditions)     // [C]
{
    TimeAndPosition time_and_position;
    time_and_position.collector_location = collector_location_;
    time_and_position.collector_orientation = collector_orientation_;
    time_and_position.timestamp = timestamp;
    double T_in = external_conditions.inlet_fluid_flow.temp;
    double T_amb = external_conditions.weather.ambient_temp;
    double m_dot = external_conditions.inlet_fluid_flow.m_dot;
    double specific_heat = external_conditions.inlet_fluid_flow.specific_heat;
    double specific_heat_capacity = m_dot * specific_heat;

    // Inlet pipe
    double T_out_inlet_pipe = inlet_pipe_.T_out(T_in, T_amb, specific_heat_capacity);
    double T_array_in = T_out_inlet_pipe;

    // Collectors
    ExternalConditions external_conditions_to_collector(external_conditions);
    external_conditions_to_collector.inlet_fluid_flow.temp = T_array_in;
    external_conditions_to_collector.inlet_fluid_flow.m_dot = m_dot / array_dimensions_.num_in_parallel;
    double T_array_out = T_array_in;
    for (std::size_t i = 0; i < array_dimensions_.num_in_series; i++) {
        double T_collector_out = flat_plate_collector_.T_out(time_and_position, external_conditions_to_collector);
        external_conditions_to_collector.inlet_fluid_flow.temp = T_collector_out;   // to next collector in series
        T_array_out = T_collector_out;
    }

    // Outlet pipe
    double T_out_outlet_pipe = outlet_pipe_.T_out(T_array_out, T_amb, specific_heat_capacity);
    return T_out_outlet_pipe;
}
