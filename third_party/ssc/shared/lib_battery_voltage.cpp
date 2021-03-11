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

#include <algorithm>
#include <functional>
#include "6par_newton.h"

#include "lib_battery_voltage.h"

/*
Define Voltage Model
*/
bool voltage_state::operator==(const voltage_state &p) {
    return cell_voltage == p.cell_voltage;
}

void voltage_t::initialize() {
    state = std::make_shared<voltage_state>();
    state->cell_voltage = params->Vnom_default;
}

voltage_t::voltage_t(int mode, int num_cells_series, int num_strings, double voltage, double dt_hour) {
    params = std::make_shared<voltage_params>();

    params->voltage_choice = static_cast<voltage_params::MODE>(mode);
    params->num_cells_series = num_cells_series;
    params->num_strings = num_strings;
    params->Vnom_default = voltage;
    params->resistance = 0.004; // just a default, will get recalculated upon construction
    params->dt_hr = dt_hour;
    initialize();
}

voltage_t::voltage_t(std::shared_ptr<voltage_params> p):
        params(std::move(p)) {
    initialize();
}

voltage_t::voltage_t(const voltage_t &rhs) {
    state = std::make_shared<voltage_state>(*rhs.state);
    params = std::make_shared<voltage_params>(*rhs.params);
}

voltage_t &voltage_t::operator=(const voltage_t &rhs) {
    if (this != &rhs) {
        *params = *rhs.params;
        *state = *rhs.state;
    }
    return *this;
}

double voltage_t::battery_voltage() { return params->num_cells_series * state->cell_voltage; }

double voltage_t::battery_voltage_nominal() { return params->num_cells_series * params->Vnom_default; }

double voltage_t::cell_voltage() { return state->cell_voltage; }

voltage_params voltage_t::get_params() { return *params; }

voltage_state voltage_t::get_state() { return *state; }

// Voltage Table

void voltage_table_t::initialize() {
    if (params->voltage_table.empty()) {
        throw std::runtime_error("voltage_table_t error: empty voltage table");
    }
    if (params->voltage_table.size() < 2 || params->voltage_table[0].size() != 2)
        throw std::runtime_error("voltage_table_t error: Battery lifetime matrix must have 2 columns and at least 2 rows");

    // save slope and intercept for every set of points to interpolate between
    std::sort(params->voltage_table.begin(), params->voltage_table.end(),
              [](std::vector<double> a, std::vector<double> b) { return a[1] > b[1]; });
    for (size_t i = 0; i != params->voltage_table.size(); i++) {
        double DOD = params->voltage_table[i][0];
        double V = params->voltage_table[i][1];
        double slope = 0;
        double intercept = V;
        if (i > 0) {
            double DOD0 = params->voltage_table[i - 1][0];
            double V0 = params->voltage_table[i - 1][1];
            slope = (V - V0) / (DOD - DOD0);
            intercept = V0 - (slope * DOD0);
        }
        slopes.emplace_back(slope);
        intercepts.emplace_back(intercept);
    }

    // for extrapolation beyond given points
    slopes.emplace_back(slopes.back());
    intercepts.emplace_back(intercepts.back());
}

voltage_table_t::voltage_table_t(int num_cells_series, int num_strings, double voltage,
                                 util::matrix_t<double> &voltage_table, double R, double dt_hour) :
        voltage_t(voltage_params::TABLE, num_cells_series, num_strings, voltage, dt_hour) {
    params->resistance = R;
    for (int r = 0; r != (int) voltage_table.nrows(); r++)
        params->voltage_table.emplace_back(std::vector<double>({voltage_table.at(r, 0), voltage_table.at(r, 1)}));
    initialize();
}

voltage_table_t::voltage_table_t(std::shared_ptr<voltage_params> p):
        voltage_t(std::move(p)){
    initialize();
}

voltage_table_t &voltage_table_t::operator=(const voltage_t &rhs) {
    if (this != &rhs) {
        voltage_t::operator=(rhs);
        auto rhs_p = dynamic_cast<voltage_table_t *>(const_cast<voltage_t *>(&rhs));
        slopes = rhs_p->slopes;
        intercepts = rhs_p->intercepts;
    }
    return *this;
}

voltage_table_t::voltage_table_t(const voltage_table_t &rhs) :
        voltage_t(rhs) {
    operator=(rhs);
}

voltage_t *voltage_table_t::clone() {
    return new voltage_table_t(*this);
}

double voltage_table_t::calculate_voltage(double DOD) {
    DOD = fmax(0., DOD);
    DOD = fmin(DOD, 100.);

    size_t row = 0;
    while (row < params->voltage_table.size() && DOD > params->voltage_table[row][0]) {
        row++;
    }

    return fmax(slopes[row] * DOD + intercepts[row], 0);
}

void voltage_table_t::set_initial_SOC(double init_soc) {
    state->cell_voltage = calculate_voltage(100. - init_soc);
}

double voltage_table_t::calculate_voltage_for_current(double I, double q, double qmax, double) {
    double DOD = (q - I * params->dt_hr) / qmax * 100.;
    return calculate_voltage(DOD) * params->num_cells_series;
}


void voltage_table_t::updateVoltage(double q, double qmax, double, const double, double) {
    double DOD = 100. * (1 - q / qmax);
    state->cell_voltage = calculate_voltage(DOD);
}

// helper fx to calculate depth of discharge from current and max capacities
inline double calc_DOD(double q, double qmax) { return (1. - q / qmax) * 100.; }

double voltage_table_t::calculate_max_charge_w(double q, double qmax, double, double *max_current) {
    double current = (q - qmax) / params->dt_hr;
    if (max_current)
        *max_current = current;
    return calculate_voltage(0.) * current * params->num_cells_series;
}

double voltage_table_t::calculate_max_discharge_w(double q, double qmax, double, double *max_current) {
    double DOD0 = calc_DOD(q, qmax);
    double A = q - qmax;
    double B = qmax / 100.;

    double max_P = 0;
    double max_I = 0;
    for (size_t i = 0; i < slopes.size(); i++) {
        double dod = -(A * slopes[i] + B * intercepts[i]) / (2 * B * slopes[i]);
        double current = qmax * ((1. - DOD0 / 100.) - (1. - dod / 100.)) / params->dt_hr;
        double p = calculate_voltage(dod) * current;
        if (p > max_P) {
            max_P = p;
            max_I = current;
        }
    }
    if (max_current)
        *max_current = fmax(0, max_I);
    return max_P * params->num_cells_series;
}

double voltage_table_t::calculate_current_for_target_w(double P_watts, double q, double qmax, double) {
    double DOD = calc_DOD(q, qmax);
    double max_p, current;
    if (P_watts == 0)
        return 0.;
    else if (P_watts < 0)
        max_p = calculate_max_charge_w(q, qmax, 0, &current);
    else
        max_p = calculate_max_discharge_w(q, qmax, 0, &current);

    if (fabs(max_p) <= fabs(P_watts))
        return current;

    P_watts /= params->num_cells_series;
    P_watts *= params->dt_hr;
    double multiplier = 1.;
    if (P_watts < 0)
        multiplier = -1.;

    size_t row = 0;
    while (row < params->voltage_table.size() && DOD > params->voltage_table[row][0]) {
        row++;
    }

    double A = q - qmax;
    double B = qmax / 100.;

    double DOD_new = 0.;
    double incr = 0;
    double DOD_best = DOD_best = multiplier == -1. ? 0 : 100;
    double P_best = 0;
    while (incr + row < slopes.size() && incr + row >= 0) {
        size_t i = row + (size_t) incr;
        incr += 1 * multiplier;

        double a = B * slopes[i];
        double b = A * slopes[i] + B * intercepts[i];
        double c = A * intercepts[i] - P_watts;

        if (a == 0) {
            continue;
        }

        DOD_new = fabs((-b + sqrt(b * b - 4 * a * c)) / (2 * a));

        auto upper = (size_t) fmin(i, params->voltage_table.size() - 1);
        auto lower = (size_t) fmax(0, i - 1);
        auto DOD_upper = params->voltage_table[upper][0];
        auto DOD_lower = params->voltage_table[lower][0];
        if (DOD_new <= DOD_upper && DOD_new >= DOD_lower) {
            double P = (q - (100. - DOD_new) * qmax/100) * (a * DOD_new + b);
            if (fabs(P) > fabs(P_best)) {
                P_best = P;
                DOD_best = DOD_new;
            }
        }
    }
    return qmax * ((1. - DOD / 100.) - (1. - DOD_best / 100.)) / params->dt_hr;
}

// Dynamic voltage model
typedef void (voltage_dynamic_t::*voltage_dynamic_fptr)(const double *, double *);

void voltage_dynamic_t::initialize() {
    if ((params->dynamic.Vfull < params->dynamic.Vexp) ||
            (params->dynamic.Vexp < params->dynamic.Vnom)) {
        throw std::runtime_error("voltage_dynamic_t error: For the electrochemical battery voltage model, voltage inputs must meet the requirement Vfull > Vexp > Vnom.");
    }
    // assume fully charged, not the nominal value
    state->cell_voltage = params->dynamic.Vfull;
    parameter_compute();
}

voltage_dynamic_t::voltage_dynamic_t(int num_cells_series, int num_strings, double voltage, double Vfull,
                                     double Vexp, double Vnom, double Qfull, double Qexp, double Qnom,
                                     double C_rate, double R, double dt_hr) :
        voltage_t(voltage_params::MODEL, num_cells_series, num_strings, voltage, dt_hr) {
    params->dynamic.Vfull = Vfull;
    params->dynamic.Vexp = Vexp;
    params->dynamic.Vnom = Vnom;
    params->dynamic.Qfull = Qfull;
    params->dynamic.Qexp = Qexp;
    params->dynamic.Qnom = Qnom;
    params->dynamic.C_rate = C_rate;
    params->resistance = R;
    initialize();
}

voltage_dynamic_t::voltage_dynamic_t(std::shared_ptr<voltage_params> p):
        voltage_t(std::move(p)){
    initialize();
}

voltage_dynamic_t &voltage_dynamic_t::operator=(const voltage_t &rhs) {
    if (this != &rhs) {
        voltage_t::operator=(rhs);

        auto rhs_p = dynamic_cast<voltage_dynamic_t *>(const_cast<voltage_t *>(&rhs));
        _A = rhs_p->_A;
        _B0 = rhs_p->_B0;
        _E0 = rhs_p->_E0;
        _K = rhs_p->_K;

        solver_power = rhs_p->solver_power;
        solver_Q = rhs_p->solver_Q;
        solver_q = rhs_p->solver_q;
        solver_cutoff_voltage = rhs_p->solver_cutoff_voltage;
    }
    return *this;
}

voltage_dynamic_t::voltage_dynamic_t(const voltage_dynamic_t &rhs) :
        voltage_t(rhs) {
    operator=(rhs);
}

voltage_t *voltage_dynamic_t::clone() {
    return new voltage_dynamic_t(*this);
}

void voltage_dynamic_t::parameter_compute() {
    // Determines parameters according to page 2 of:
    // Tremblay 2009 "A Generic Bettery Model for the Dynamic Simulation of Hybrid Electric Vehicles"
//	double eta = 0.995;
    double I = params->dynamic.Qfull * params->dynamic.C_rate; // [A]
    //_R = params->dynamic.Vnom*(1. - eta) / (params->dynamic.C_rate*params->dynamic.Qnom); // [Ohm]
    _A = params->dynamic.Vfull - params->dynamic.Vexp; // [V]
    _B0 = 3. / params->dynamic.Qexp;     // [1/Ah]
    _K = ((params->dynamic.Vfull - params->dynamic.Vnom + _A * (std::exp(-_B0 * params->dynamic.Qnom) - 1)) *
          (params->dynamic.Qfull - params->dynamic.Qnom)) / (params->dynamic.Qnom); // [V] - polarization voltage
    _E0 = params->dynamic.Vfull + _K + params->resistance * I - _A;

    if (_A < 0 || _B0 < 0 || _K < 0 || _E0 < 0) {
        char err[254];
        std::sprintf(err, "Error during calculation of battery voltage model parameters: negative value(s) found.\n"
                          "A: %f, B: %f, K: %f, E0: %f", _A, _B0, _K, _E0);
        throw std::runtime_error(err);
    }
}

void voltage_dynamic_t::set_initial_SOC(double init_soc) {
    updateVoltage(init_soc * 0.01 * params->dynamic.Qfull * params->num_strings, params->dynamic.Qfull * params->num_strings, 0, 25, params->dt_hr);
}

// everything in here is on a per-cell basis
double voltage_dynamic_t::voltage_model_tremblay_hybrid(double Q_cell, double I, double q0_cell) {
    double it = Q_cell - q0_cell;
    double E = _E0 - _K * (Q_cell / (Q_cell - it)) + _A * exp(-_B0 * it);
    return E - params->resistance * I;
}

double voltage_dynamic_t::calculate_voltage_for_current(double I, double q, double qmax, double) {
    return params->num_cells_series *
           fmax(voltage_model_tremblay_hybrid(qmax / params->num_strings, I / params->num_strings,
                                              q / params->num_strings), 0);
}

// I, Q, q0 are on a per-string basis since adding cells in series does not change current or charge
void voltage_dynamic_t::updateVoltage(double q, double qmax, double I, const double, double) {
    qmax /= params->num_strings;
    q /= params->num_strings;
    I /= params->num_strings;

    state->cell_voltage = fmax(voltage_model_tremblay_hybrid(qmax, I, q), 0);
}

double voltage_dynamic_t::calculate_max_charge_w(double q, double qmax, double, double *max_current) {
    q /= params->num_strings;
    qmax /= params->num_strings;
    double current = (q - qmax) / params->dt_hr;
    if (max_current)
        *max_current = current * params->num_strings;
    return current * voltage_model_tremblay_hybrid(qmax, current, qmax) * params->num_strings *
           params->num_cells_series;
}

using namespace std::placeholders;

double voltage_dynamic_t::calculate_max_discharge_w(double q, double qmax, double, double *max_current) {
    q /= params->num_strings;
    qmax /= params->num_strings;

    double current = q * 0.5, vol = 0;
    double incr = q / 10;
    double max_p = 0, max_I = 0;
    while (current * params->dt_hr < q - tolerance && vol >= 0) {
        vol = voltage_model_tremblay_hybrid(qmax, current, q - current * params->dt_hr);
        double p = current * vol;
        if (p > max_p) {
            max_p = p;
            max_I = current;
        }
        current += incr;
    }
    current = max_I;

    if (max_current)
        *max_current = current * params->num_strings;

    return max_p * params->num_strings * params->num_cells_series;
}

double voltage_dynamic_t::calculate_current_for_target_w(double P_watts, double q, double qmax, double) {
    if (P_watts == 0) return 0.;

    solver_power = fabs(P_watts) / (params->num_cells_series * params->num_strings);
    solver_q = q / params->num_strings;
    solver_Q = qmax / params->num_strings;

    std::function<void(const double *, double *)> f;
    double direction = 1.;
    if (P_watts > 0)
        f = std::bind(&voltage_dynamic_t::solve_current_for_discharge_power, this, _1, _2);
    else {
        f = std::bind(&voltage_dynamic_t::solve_current_for_charge_power, this, _1, _2);
        direction = -1.;
    }

    double x[1], resid[1];
    if (state->cell_voltage != 0)
        x[0] = solver_power / state->cell_voltage * params->dt_hr;
    else
        x[0] = solver_power / params->dynamic.Vnom * params->dt_hr;
    bool check = false;

    newton<double, std::function<void(const double *, double *)>, 1>(x, resid, check, f,
                                                                     100, 1e-6, 1e-6, 0.7);
    return x[0] * params->num_strings * direction;
}

void voltage_dynamic_t::solve_current_for_charge_power(const double *x, double *f) {
    double I = x[0];
    double V = _E0 - _K * solver_Q / (solver_q + I * params->dt_hr) +
               _A * exp(-_B0 * (solver_Q - (solver_q + I * params->dt_hr))) + params->resistance * I;
    f[0] = I * V - solver_power;
}

void voltage_dynamic_t::solve_current_for_discharge_power(const double *x, double *f) {
    double I = x[0];
    double V = _E0 - _K * solver_Q / (solver_q - I * params->dt_hr) +
               _A * exp(-_B0 * (solver_Q - (solver_q - I * params->dt_hr))) - params->resistance * I;
    f[0] = I * V - solver_power;
}

// Vanadium redox flow model
void voltage_vanadium_redox_t::initialize() {
    m_RCF = 8.314 * 1.38 / (26.801 * 3600);
}

voltage_vanadium_redox_t::voltage_vanadium_redox_t(int num_cells_series, int num_strings, double Vnom_default,
                                                   double R, double dt_hour) :
        voltage_t(voltage_params::MODEL, num_cells_series, num_strings, Vnom_default, dt_hour) {
    params->Vnom_default = Vnom_default;
    params->resistance = R;
    params->dt_hr = params->dt_hr;
    initialize();
}

voltage_vanadium_redox_t::voltage_vanadium_redox_t(std::shared_ptr<voltage_params> p):
        voltage_t(std::move(p)){
    initialize();
}

voltage_vanadium_redox_t &voltage_vanadium_redox_t::operator=(const voltage_t &rhs) {
    if (this != &rhs) {
        voltage_t::operator=(rhs);

        auto rhs_p = dynamic_cast<voltage_vanadium_redox_t *>(const_cast<voltage_t *>(&rhs));
        m_RCF = rhs_p->m_RCF;
        solver_power = rhs_p->solver_power;
        solver_T_k = rhs_p->solver_T_k;
        solver_q = rhs_p->solver_q;
        solver_Q = rhs_p->solver_Q;
    }
    return *this;
}

voltage_vanadium_redox_t::voltage_vanadium_redox_t(const voltage_vanadium_redox_t &rhs) :
        voltage_t(rhs) {
    operator=(rhs);
}

voltage_t *voltage_vanadium_redox_t::clone() {
    return new voltage_vanadium_redox_t(*this);
}

void voltage_vanadium_redox_t::set_initial_SOC(double init_soc) {
    updateVoltage(init_soc, 100, 0, 25, params->dt_hr);
}


double voltage_vanadium_redox_t::calculate_voltage_for_current(double I, double q, double qmax, double T_k) {
    return voltage_model(q / params->num_strings, qmax / params->num_strings,
                         I / params->num_strings, T_k) * params->num_cells_series;
}

void voltage_vanadium_redox_t::updateVoltage(double q, double qmax, double I, const double temp, double) {
    state->cell_voltage = voltage_model(q / params->num_strings, qmax / params->num_strings,
                                       I / params->num_strings, temp + 273.15);
}

double voltage_vanadium_redox_t::calculate_max_charge_w(double q, double qmax, double kelvin, double *max_current) {
    qmax /= params->num_strings;
    q /= params->num_strings;
    double max_I = (q - qmax) / params->dt_hr;

    if (max_current)
        *max_current = max_I * params->num_strings;

    return voltage_model(qmax, qmax, max_I, kelvin) * max_I * params->num_strings * params->num_cells_series;
}

double voltage_vanadium_redox_t::calculate_max_discharge_w(double q, double qmax, double kelvin, double *max_current) {

    solver_q = q / params->num_strings;
    solver_Q = qmax / params->num_strings;
    solver_T_k = kelvin;

    std::function<void(const double *, double *)> f = std::bind(&voltage_vanadium_redox_t::solve_max_discharge_power,
                                                                this, _1, _2);

    double x[1], resid[1];
    x[0] = (solver_q - tolerance) / params->dt_hr;
    bool check = false;

    newton<double, std::function<void(const double *, double *)>, 1>(x, resid, check, f,
                                                                     100, 1e-6, 1e-6, 0.7);
    double current = x[0];

    double power = current * voltage_model(solver_q - current * params->dt_hr, solver_Q, current, kelvin) *
                   params->num_strings * params->num_cells_series;

    if (power < 0) {
        current = 0.;
        power = 0.;
    }
    if (max_current)
        *max_current = current * params->num_strings;
    return power;
}

double voltage_vanadium_redox_t::calculate_current_for_target_w(double P_watts, double q, double qmax, double kelvin) {
    if (P_watts == 0) return 0.;

    solver_power = P_watts / (params->num_cells_series * params->num_strings);
    solver_q = q / params->num_strings;
    solver_Q = qmax / params->num_strings;
    solver_T_k = kelvin;

    std::function<void(const double *, double *)> f = std::bind(&voltage_vanadium_redox_t::solve_current_for_power,
                                                                this, _1, _2);

    double x[1], resid[1];
    if (state->cell_voltage != 0.)
        x[0] = solver_power / state->cell_voltage * params->dt_hr;
    else
        x[0] = solver_power / params->Vnom_default * params->dt_hr;
    bool check = false;

    newton<double, std::function<void(const double *, double *)>, 1>(x, resid, check, f,
                                                                     100, 1e-6, 1e-6, 0.7);
    return x[0] * params->num_strings;
}

// I, Q, q0 are on a per-string basis since adding cells in series does not change current or charge
// In constrast to the V_stack + I_stack * R_specific in the paper which follows the convention of negative voltages,
// here the abs(I_stack) is used to allow both terms to move in same direction (https://github.com/NREL/ssc/issues/404)
double voltage_vanadium_redox_t::voltage_model(double q0, double qmax, double I_string, double T) {
    double SOC_use = q0 / qmax;
    if (SOC_use > 1. - tolerance)
        SOC_use = 1. - tolerance;
    else if (SOC_use == 0)
        SOC_use = 1e-3;

    double A = std::log(std::pow(SOC_use, 2) / std::pow(1 - SOC_use, 2));

    return params->Vnom_default + m_RCF * T * A + fabs(I_string) * params->resistance;
}

void voltage_vanadium_redox_t::solve_current_for_power(const double *x, double *f) {
    double I = x[0];
    double SOC = (solver_q - I * params->dt_hr) / solver_Q;
    f[0] = I * (params->Vnom_default + m_RCF * solver_T_k * std::log(SOC * SOC / std::pow(1. - SOC, 2)) +
                fabs(I) * params->resistance) - solver_power;
}

void voltage_vanadium_redox_t::solve_max_discharge_power(const double *x, double *f) {
    double I = fabs(x[0]);
    double SOC = (solver_q - I * params->dt_hr) / solver_Q;
    f[0] = params->Vnom_default + 2 * I * params->resistance + m_RCF * solver_T_k *
                                                               (std::log(SOC * SOC / pow(1. - SOC, 2)) -
                                                               2 * I * (1. / SOC - 1. / (1. - SOC)));
}
