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


#include <cmath>

#include "lib_battery_lifetime_calendar_cycle.h"
#include "lib_battery_lifetime_nmc.h"
#include "lib_battery_lifetime.h"
#include "logger.h"

void lifetime_nmc_t::initialize() {
    state = std::make_shared<lifetime_state>();
    // cycle model for counting cycles only, no cycle-only degradation
    cycle_model = std::unique_ptr<lifetime_cycle_t>(new lifetime_cycle_t(params, state));
    // do any state initialization here
    state->nmc_li_neg->dq_relative_li_old = 0;
    state->nmc_li_neg->dq_relative_neg_old = 0;
    state->nmc_li_neg->DOD_max = 0;
    state->nmc_li_neg->n_cycles_prev_day = 0;
    state->nmc_li_neg->cum_dt = 0;
    state->nmc_li_neg->b1_dt = b1_ref;
    state->nmc_li_neg->b2_dt = b2_ref;
    state->nmc_li_neg->b3_dt = b3_ref;
    state->nmc_li_neg->q_relative_li = runQli(T_ref);
    state->nmc_li_neg->c0_dt = c0_ref;
    state->nmc_li_neg->c2_dt = c2_ref;
    state->nmc_li_neg->q_relative_neg = runQneg();
    state->q_relative = fmin(state->nmc_li_neg->q_relative_li, state->nmc_li_neg->q_relative_neg);
}

lifetime_nmc_t::lifetime_nmc_t(double dt_hr) {
    params = std::make_shared<lifetime_params>();
    params->model_choice = lifetime_params::NMCNREL;
    params->dt_hr = dt_hr;
    initialize();
}

lifetime_nmc_t::lifetime_nmc_t(std::shared_ptr<lifetime_params> params_pt) {
    params = std::move(params_pt);
    initialize();
}

lifetime_nmc_t::lifetime_nmc_t(std::shared_ptr<lifetime_params> params_pt, std::shared_ptr<lifetime_state> state_pt) {
    params = std::move(params_pt);
    state = std::move(state_pt);
    cycle_model = std::unique_ptr<lifetime_cycle_t>(new lifetime_cycle_t(params, state));
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

//calculate_Uneg, and calculate_Voc are picewise linear functions. The linear functions were obtained by using values
//from  Table B.1 of the applied energy paper " Analysis of degradation in residential battery energy storate systems
// for rate-based use-cases"

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

double lifetime_nmc_t::runQli(double T_battery_K) {
    double dt_day = 1;
    int dn_cycles = state->n_cycles - state->nmc_li_neg->n_cycles_prev_day;
    double b1 = state->nmc_li_neg->b1_dt;
    double b2 = state->nmc_li_neg->b2_dt;
    double b3 = state->nmc_li_neg->b3_dt;

    state->nmc_li_neg->b1_dt = 0;
    state->nmc_li_neg->b2_dt = 0;
    state->nmc_li_neg->b3_dt = 0;

    // Reversible thermal capacity dependence
    double d0_t = d0_ref * exp(-(Ea_d0_1 / Rug) * (1 / T_battery_K - 1 / T_ref) -
                      (Ea_d0_2 / Rug) * pow(1 / T_battery_K - 1 / T_ref, 2));

    double k_cal = 0;
    if (state->day_age_of_battery > 0)
        k_cal = (0.5 * b1) / sqrt(state->day_age_of_battery) + (b3 / tau_b3) * exp(-(state->day_age_of_battery / tau_b3));

    double dq_new = k_cal * dt_day + b2 * dn_cycles + state->nmc_li_neg->dq_relative_li_old;
    state->nmc_li_neg->dq_relative_li_old = dq_new;
    state->nmc_li_neg->q_relative_li = d0_t / Ah_ref * (b0 - dq_new) * 100.;
    return state->nmc_li_neg->q_relative_li;
}

double lifetime_nmc_t::runQneg() {

    int dn_cycles = state->n_cycles - state->nmc_li_neg->n_cycles_prev_day;

    double c0 = state->nmc_li_neg->c0_dt;
    double c2 = state->nmc_li_neg->c2_dt;
    state->nmc_li_neg->c0_dt = 0;
    state->nmc_li_neg->c2_dt = 0;

    double dq_new = 0;
    if (state->n_cycles > 0)
        dq_new = c2 / sqrt(c0 * c0 - 2 * c2 * c0 * state->n_cycles) * dn_cycles + state->nmc_li_neg->dq_relative_neg_old;

    state->nmc_li_neg->dq_relative_neg_old = dq_new;
    state->nmc_li_neg->q_relative_neg = c0 / Ah_ref * (1 - dq_new) * 100;

    return state->nmc_li_neg->q_relative_neg;
}

void lifetime_nmc_t::integrateDegParams(double dt_day, double DOD, double T_battery) {
    //compute open circuit and negative electrode voltage as function of SOC
    double SOC = 0.01 * (100 - DOD);
    double DOD_max = state->nmc_li_neg->DOD_max * 0.01;
    double U_neg = calculate_Uneg(SOC);
    double V_oc = calculate_Voc(SOC);

    // multiply by timestep in days and populate corresponding vectors
    double b1_dt_el = b1_ref * exp(-(Ea_b1 / Rug) * (1. / T_battery - 1. / T_ref))
                      * exp((alpha_a_b1 * F / Rug) * (U_neg / T_battery - Uneg_ref / T_ref))
                      * exp(gamma * pow(DOD_max, beta_b1)) * dt_day;
    double b2_dt_el = b2_ref * exp(-(Ea_b_2 / Rug) * (1. / T_battery - 1. / T_ref)) * dt_day;
    double b3_dt_el = b3_ref * exp(-(Ea_b3 / Rug) * (1. / T_battery - 1. / T_ref))
                      * exp((alpha_a_b3 * F / Rug) * (V_oc / T_battery - V_ref / T_ref))
                      * (1 + theta * DOD_max) * dt_day;

    state->nmc_li_neg->b1_dt += b1_dt_el;
    state->nmc_li_neg->b2_dt += b2_dt_el;
    state->nmc_li_neg->b3_dt += b3_dt_el;

    // computations for q_neg
    double c2_dt_el = c2_ref * exp(-(Ea_c2 / Rug) * (1. / T_battery - 1. / T_ref))
                      * pow(0.01 * state->nmc_li_neg->DOD_max, beta_c2) * dt_day;
    double c0_dt_el = c0_ref * exp(-Ea_c0_ref / Rug * (1 / T_battery - 1 / T_ref)) * dt_day;
    state->nmc_li_neg->c0_dt += c0_dt_el;
    state->nmc_li_neg->c2_dt += c2_dt_el;

    state->nmc_li_neg->cum_dt += dt_day;
}

void lifetime_nmc_t::integrateDegLoss(double DOD, double T_battery) {
    state->nmc_li_neg->q_relative_li = runQli(T_battery);
    state->nmc_li_neg->q_relative_neg = runQneg();
    state->q_relative = fmin(state->nmc_li_neg->q_relative_li, state->nmc_li_neg->q_relative_neg);

    // reset DOD_max for cycle tracking
    state->nmc_li_neg->cum_dt = 0;
    if (state->n_cycles - state->nmc_li_neg->n_cycles_prev_day > 0)
        state->nmc_li_neg->DOD_max = DOD;
    state->nmc_li_neg->n_cycles_prev_day = state->n_cycles;
}

void lifetime_nmc_t::runLifetimeModels(size_t _, bool charge_changed, double prev_DOD, double DOD,
                                       double T_battery) {
    double q_last = state->q_relative;
    // convert battery temperature to Kelvin
    T_battery += 273.15;
    if (charge_changed)
        cycle_model->rainflow(prev_DOD);

    double dt_day = (1. / (double)util::hours_per_day) * params->dt_hr;
    // Run capacity degradation model after every 24 hours
    double new_cum_dt = state->nmc_li_neg->cum_dt + dt_day;
    if (new_cum_dt > 1 + 1e-7) {
        double dt_day_to_end_of_day = 1 - state->nmc_li_neg->cum_dt;
        double DOD_at_end_of_day = (DOD - prev_DOD) / dt_day * dt_day_to_end_of_day + prev_DOD;
        state->nmc_li_neg->DOD_max = fmax(DOD_at_end_of_day, state->nmc_li_neg->DOD_max);
        state->day_age_of_battery += dt_day_to_end_of_day;

        integrateDegParams(dt_day_to_end_of_day, DOD_at_end_of_day, T_battery);
        integrateDegLoss(DOD_at_end_of_day, T_battery);

        dt_day = new_cum_dt - 1;
    }

    state->nmc_li_neg->DOD_max = fmax(DOD, state->nmc_li_neg->DOD_max);
    state->day_age_of_battery += dt_day;
    integrateDegParams(dt_day, DOD, T_battery);

    if (fabs(state->nmc_li_neg->cum_dt - 1.) < 1e-7) {
        integrateDegLoss(DOD, T_battery);
    }

    state->q_relative = fmin(state->q_relative, q_last);
}

double lifetime_nmc_t::estimateCycleDamage() {
    // Assume T_battery is at T_ref and use average range
    double c2 = c2_ref * pow(0.01 * state->average_range, beta_c2);
    double dq_cycle = c2 / sqrt(c0_ref * c0_ref - 2 * c2 * c0_ref * state->n_cycles);
    return c0_ref / Ah_ref * dq_cycle * 100;
}

void lifetime_nmc_t::replaceBattery(double percent_to_replace) {
    state->day_age_of_battery = 0;
    state->nmc_li_neg->dq_relative_li_old = 0;
    state->nmc_li_neg->dq_relative_neg_old = 0;
    state->nmc_li_neg->q_relative_li += percent_to_replace;
    state->nmc_li_neg->q_relative_neg += percent_to_replace;
    state->nmc_li_neg->q_relative_li = fmin(100, state->nmc_li_neg->q_relative_li);
    state->nmc_li_neg->q_relative_neg = fmin(100, state->nmc_li_neg->q_relative_neg);
    state->q_relative = fmin(state->nmc_li_neg->q_relative_li, state->nmc_li_neg->q_relative_neg);
}
