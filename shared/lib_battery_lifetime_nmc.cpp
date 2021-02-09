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


#include "lib_battery_lifetime_calendar_cycle.h"
#include "lib_battery_lifetime_nmc.h"
#include "lib_battery_lifetime.h"
#include <numeric>
#include <cmath>

void lifetime_nmc_t::initialize() {

    // cycle model for counting cycles only, no cycle-only degradation
    cycle_model = std::unique_ptr<lifetime_cycle_t>(new lifetime_cycle_t(params, state));
    // do any state initialization here
    state->q_relative = 100;
    state->nmc_state->q_relative_li = 100;
    state->nmc_state->q_relative_neg = 100;
    state->nmc_state->dq_relative_li_old = 0;
    state->nmc_state->dq_relative_neg_old = 0;
    state->nmc_state->DOD_max = 50;
    state->nmc_state->n_cycles_prev_day = 0;
    state->nmc_state->b1_dt = 0;
    state->nmc_state->b2_dt = 0;
    state->nmc_state->b3_dt = 0;
    state->nmc_state->c2_dt = 0;
}

lifetime_nmc_t::lifetime_nmc_t(double dt_hr) {
    params = std::make_shared<lifetime_params>();
    params->model_choice = lifetime_params::NMCNREL;
    params->dt_hr = dt_hr;
    state = std::make_shared<lifetime_state>();
    initialize();
}

lifetime_nmc_t::lifetime_nmc_t(std::shared_ptr<lifetime_params> params_pt) {
    params = std::move(params_pt);
    initialize();
}

lifetime_nmc_t::lifetime_nmc_t(const lifetime_nmc_t &rhs) :
        lifetime_t(rhs){
    operator=(rhs);
}

lifetime_nmc_t& lifetime_nmc_t::operator=(const lifetime_nmc_t& rhs) {
    if (this != &rhs) {
        *params = *rhs.params;
        *state = *rhs.state;
    }
    return *this;
}

lifetime_t * lifetime_nmc_t::clone() {
    return new lifetime_nmc_t(*this);
}

double lifetime_nmc_t::calculate_Uneg(double SOC) {
    double Uneg;
    if (SOC <= 0.1)
        Uneg = ((0.2420 - 1.2868) / 0.1) * SOC + 1.2868;
    else
        Uneg = ((0.0859 - 0.2420) / 0.9) * (SOC - 0.1) + 0.2420;
    return Uneg;
}

double lifetime_nmc_t::calculate_Voc(double SOC) {
    double Voc;
    if (SOC <= 0.1)
        Voc = ((0.4679) / 0.1) * SOC + 3;
    else if (SOC <= 0.6)
        Voc = ((3.747 - 3.4679) / 0.5) * (SOC - 0.1) + 3.4679;
    else
        Voc = ((4.1934 - 3.7469) / 0.4) * (SOC - 0.6) + 3.7469;
    return Voc;
}

double lifetime_nmc_t::runQli() {
    double dt_day = 1;
    int dn_cycles = state->n_cycles - state->nmc_state->n_cycles_prev_day;
    double k_cal = 0;
    //double b1 = std::accumulate(state->nmc_state->b1_dt.begin(), state->nmc_state->b1_dt.end(), 0);
    double b1 = state->nmc_state->b1_dt;
    double b2 = state->nmc_state->b2_dt;
    double b3 = state->nmc_state->b3_dt;

    state->nmc_state->b1_dt = 0;
    state->nmc_state->b2_dt = 0;
    state->nmc_state->b3_dt = 0;

    if (state->day_age_of_battery > 0)
        k_cal = (0.5 * b1) / (sqrt(state->day_age_of_battery)) + (b3 / tau_b3) * exp(-(state->day_age_of_battery / tau_b3));
    else
        k_cal = 0;

    double dq_new;
    if (state->nmc_state->dq_relative_li_old == 0)
        dq_new = k_cal * dt_day + b2 * dn_cycles;
    else
        dq_new = k_cal * dt_day + b2 * dn_cycles + state->nmc_state->dq_relative_li_old;
    state->nmc_state->dq_relative_li_old = dq_new;
    state->nmc_state->q_relative_li = (1.07 - (dq_new)) * 100;
    return state->nmc_state->q_relative_li;
}

double lifetime_nmc_t::runQneg(double T_battery, double SOC) {

    int dn_cycles = state->n_cycles - state->nmc_state->n_cycles_prev_day;

    double c2 = state->nmc_state->c2_dt;
    state->nmc_state->c2_dt = 0;

    double dq_new;
    if (state->nmc_state->dq_relative_neg_old == 0)
        dq_new = 1 - sqrt(1 - 2 * (c2 / c0_ref) * dn_cycles);
    else
        dq_new = 1 - sqrt(1 - 2 * (c2 / c0_ref) * dn_cycles) + state->nmc_state->dq_relative_neg_old;

    state->nmc_state->dq_relative_neg_old = dq_new;

    state->nmc_state->q_relative_neg = (1 - (dq_new)) * 100;

    //return state->nmc_state->q_relative_neg;
    return 100;
}

void lifetime_nmc_t::runLifetimeModels(size_t lifetimeIndex, bool charge_changed, double prev_DOD, double DOD,
                                       double T_battery) {
    double q_last = state->q_relative;

    // update day age of battery
    size_t day_age_of_battery_old = (size_t)(state->day_age_of_battery);
    state->day_age_of_battery += params->dt_hr / (double)util::hours_per_day;
    auto ts_per_day = (size_t)(util::hours_per_day / params->dt_hr);

    // convert battery temperature to Kelvin
    T_battery += 273;
    if (charge_changed)
        cycle_model->rainflow(prev_DOD);

    // update DOD_max if DOD > old DOD_max
    if (DOD > state->nmc_state->DOD_max)
        state->nmc_state->DOD_max = DOD;

    //compute open circuit and negative electrode voltage as function of SOC
    double SOC = 0.01 * (100 - DOD);
    double DOD_max = state->nmc_state->DOD_max * 0.01;
    double U_neg = calculate_Uneg(SOC);
    double V_oc = calculate_Voc(SOC);

    // compute lifetime degradation coefficients for current time step,
    //multiply by timestep in days and populate corresponding vectors
    double dt_day = (1. / 24) * params->dt_hr;
    double b1_dt_el = b1_ref * exp(-(Ea_b_1 / Rug) * (1. / T_battery - 1. / T_ref))
        * exp((alpha_a_b1 * F / Rug) * (U_neg / T_battery - U_ref / T_ref))
        * exp(gamma * pow(DOD_max, beta_b1)) * dt_day;
    double b2_dt_el = b2_ref * exp(-(Ea_b_2 / Rug) * (1. / T_battery - 1. / T_ref)) * dt_day;
    double b3_dt_el = b3_ref * exp(-(Ea_b_3 / Rug) * (1. / T_battery - 1. / T_ref))
        * exp((alpha_a_b3 * F / Rug) * (V_oc / T_battery - V_ref / T_ref))
        * (1 + theta * DOD_max);
    state->nmc_state->b1_dt += b1_dt_el;
    state->nmc_state->b2_dt += b2_dt_el;
    state->nmc_state->b3_dt += b3_dt_el;

    //computations for q_neg
    double c2_dt_el = c2_ref * exp(-(Ea_c_2 / Rug) * (1. / T_battery - 1. / T_ref))
        * pow(0.01 * DOD, beta_c2);
    state->nmc_state->c2_dt += c2_dt_el;

    //Run capacity degradation model after every 24 hours
    if (lifetimeIndex % ts_per_day == 23) {
        state->nmc_state->q_relative_li = runQli();
        state->nmc_state->q_relative_neg = runQneg(T_battery, SOC);
        state->q_relative = fmin(state->nmc_state->q_relative_li, state->nmc_state->q_relative_neg);
        state->nmc_state->n_cycles_prev_day = state->n_cycles;
//        printf("%zu, %f, %zu, %f, %f, %f\n", lifetimeIndex, state->day_age_of_battery, (size_t)(day_age_of_battery_old), state->nmc_state->q_relative_li, state->nmc_state->q_relative_neg, state->q_relative);
    }
//    else
//        printf("%zu, %f, %zu, %zu\n", lifetimeIndex, state->day_age_of_battery, (size_t)state->day_age_of_battery, day_age_of_battery_old);

    state->q_relative = fmin(state->q_relative, q_last);
}

double lifetime_nmc_t::estimateCycleDamage() {
    return 0;
}

void lifetime_nmc_t::replaceBattery(double percent_to_replace) {
    state->day_age_of_battery = 0;
    state->nmc_state->dq_relative_li_old = 0;
    state->nmc_state->dq_relative_neg_old = 0;
    state->nmc_state->q_relative_li += percent_to_replace;
    state->nmc_state->q_relative_neg += percent_to_replace;
    state->nmc_state->q_relative_li = fmin(100, state->nmc_state->q_relative_li);
    state->nmc_state->q_relative_neg = fmin(100, state->nmc_state->q_relative_neg);
    state->q_relative = fmin(state->nmc_state->q_relative_li, state->nmc_state->q_relative_neg);
}
