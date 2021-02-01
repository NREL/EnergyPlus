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
#include <stdexcept>

#include "lib_battery_capacity.h"

double low_tolerance = 0.01;
double tolerance = 0.002;

/*
Define Capacity Model
*/

bool capacity_state::operator==(const capacity_state &p) {
    bool equal = (q0 == p.q0);
    equal &= (qmax_lifetime == p.qmax_lifetime);
    equal &= (qmax_thermal == p.qmax_thermal);
    equal &= (cell_current == p.cell_current);
    equal &= (I_loss == p.I_loss);
    equal &= (SOC == p.SOC);
    equal &= (SOC_prev == p.SOC_prev);
    equal &= (charge_mode == p.charge_mode);
    equal &= (prev_charge == p.prev_charge);
    equal &= (chargeChange == p.chargeChange);
    equal &= (leadacid.q1_0 == p.leadacid.q1_0);
    equal &= (leadacid.q2_0 == p.leadacid.q2_0);
    equal &= (leadacid.q1 == p.leadacid.q1);
    equal &= (leadacid.q2 == p.leadacid.q2);
    return equal;
}

void capacity_t::initialize() {
    state = std::make_shared<capacity_state>();
    state->q0 = 0.01 * params->initial_SOC * params->qmax_init;
    state->qmax_lifetime = params->qmax_init;
    state->qmax_thermal = params->qmax_init;
    state->cell_current = 0.;
    state->I_loss = 0.;
    state->SOC = params->initial_SOC;
    state->SOC_prev = 0;

    // Initialize charging states
    state->prev_charge = capacity_state::DISCHARGE;
    state->charge_mode = capacity_state::DISCHARGE;
    state->chargeChange = false;
}

capacity_t::capacity_t() {
    params = std::make_shared<capacity_params>();
    initialize();
}

capacity_t::capacity_t(double q, double SOC_init, double SOC_max, double SOC_min, double dt_hour) :
        capacity_t() {
    params->qmax_init = q;
    params->dt_hr = dt_hour;
    params->initial_SOC = SOC_init;
    params->maximum_SOC = SOC_max;
    params->minimum_SOC = SOC_min;
    initialize();
}

capacity_t::capacity_t(std::shared_ptr<capacity_params> p) {
    params = std::move(p);
    if ((params->initial_SOC < 0 || params->initial_SOC > 100) ||
        (params->maximum_SOC < 0 || params->maximum_SOC > 100) ||
        (params->minimum_SOC < 0 || params->minimum_SOC > 100)) {
        throw std::runtime_error("Initial, Max and Min state-of-charge % must be [0, 100]");
    }
    initialize();
}

capacity_t::capacity_t(const capacity_t &rhs) {
    state = std::make_shared<capacity_state>(*rhs.state);
    params = std::make_shared<capacity_params>(*rhs.params);
}

capacity_t &capacity_t::operator=(const capacity_t &rhs) {
    if (this != &rhs) {
        *params = *rhs.params;
        *state = *rhs.state;
    }
    return *this;
}

void capacity_t::check_charge_change() {
    state->charge_mode = capacity_state::NO_CHARGE;

    // charge state
    if (state->cell_current < 0)
        state->charge_mode = capacity_state::CHARGE;
    else if (state->cell_current > 0)
        state->charge_mode = capacity_state::DISCHARGE;

    // Check if charge changed
    state->chargeChange = false;
    if ((state->charge_mode != state->prev_charge) && (state->charge_mode != capacity_state::NO_CHARGE) &&
        (state->prev_charge != capacity_state::NO_CHARGE)) {
        state->chargeChange = true;
        state->prev_charge = state->charge_mode;
    }
}

int capacity_t::charge_operation() { return state->charge_mode; }

capacity_params capacity_t::get_params() { return *params; }

capacity_state capacity_t::get_state() { return *state; }

void capacity_t::check_SOC() {
    double q_upper = state->qmax_lifetime * params->maximum_SOC * 0.01;
    double q_lower = state->qmax_lifetime * params->minimum_SOC * 0.01;

    // set capacity to upper thermal limit
    if (q_upper > state->qmax_thermal * params->maximum_SOC * 0.01) {
        q_upper = state->qmax_thermal * params->maximum_SOC * 0.01;
    }
    // do this so battery can cycle full depth and we calculate correct SOC min
    if (q_lower > state->qmax_thermal * params->minimum_SOC * 0.01) {
        q_lower = state->qmax_thermal * params->minimum_SOC * 0.01;
    }

    if (state->q0 > q_upper + tolerance) {
        // if overcharged then reduce charging
        if (state->cell_current < -tolerance) {
            state->cell_current += (state->q0 - q_upper) / params->dt_hr;

            // do not switch to discharging
            state->cell_current = fmin(0, state->cell_current);
        }
        state->q0 = q_upper;
    }
    else if (state->q0 < q_lower - tolerance) {
        // if undercharged then reduce discharhing
        if (state->cell_current > tolerance) {
            state->cell_current += (state->q0 - q_lower) / params->dt_hr;

            // do not switch to charging
            state->cell_current = fmax(0, state->cell_current);
        }
        state->q0 = q_lower;
    }
}

void capacity_t::update_SOC() {
    double max = fmin(state->qmax_lifetime, state->qmax_thermal);
    if (max == 0) {
        state->q0 = 0;
        state->SOC = 0;
        return;
    }
    if (state->q0 > max)
        state->q0 = max;
    if (state->qmax_lifetime > 0)
        state->SOC = 100. * (state->q0 / max);
    else
        state->SOC = 0.;

    // due to dynamics, it's possible SOC could be slightly above 1 or below 0
    if (state->SOC > 100.0)
        state->SOC = 100.0;
    else if (state->SOC < 0.)
        state->SOC = 0.;

}

bool capacity_t::chargeChanged() { return state->chargeChange; }

double capacity_t::SOC_max() { return params->maximum_SOC; }

double capacity_t::SOC_min() { return params->minimum_SOC; }

double capacity_t::SOC() { return state->SOC; }

double capacity_t::SOC_prev() { return state->SOC_prev; }

double capacity_t::q0() { return state->q0; }

double capacity_t::qmax() { return state->qmax_lifetime; }

double capacity_t::qmax_thermal() { return state->qmax_thermal; }

double capacity_t::I() { return state->cell_current; }

double capacity_t::I_loss() { return state->I_loss; }

/*
Define KiBam Capacity Model
*/
void capacity_kibam_t::initialize() {
    params->leadacid.t2 = 10.;
    params->leadacid.F1 = params->leadacid.qn / params->leadacid.q20; // use t1, 20
    params->leadacid.F2 = params->leadacid.qn / params->leadacid.q10;  // use t1, 10
    params->leadacid.I20 = params->leadacid.q20 / 20.;
    state->leadacid.q1 = params->leadacid.qn;
    state->leadacid.q2 = params->leadacid.q10;

    // compute the parameters
    parameter_compute();
    state->qmax_thermal = state->qmax_lifetime;
    params->qmax_init = state->qmax_lifetime;
    state->q0 = params->qmax_init * params->initial_SOC * 0.01;

    // initializes to full battery
    capacity_kibam_t::replace_battery(100);
};

capacity_kibam_t::capacity_kibam_t(double q20, double t1, double q1, double q10, double SOC_init, double SOC_max,
                                   double SOC_min, double dt_hr) :
        capacity_t(q20, SOC_init, SOC_max, SOC_min, dt_hr) {
    params->leadacid.tn = t1;
    params->leadacid.qn = q1;
    params->leadacid.q10 = q10;
    params->leadacid.q20 = q20;
    initialize();
}

capacity_kibam_t &capacity_kibam_t::operator=(const capacity_t &rhs) {
    if (this != &rhs) {
        capacity_t::operator=(rhs);
        auto rhs_p = dynamic_cast<capacity_kibam_t *>(const_cast<capacity_t *>(&rhs));
        c = rhs_p->c;
        k = rhs_p->k;
    }
    return *this;
}

capacity_kibam_t::capacity_kibam_t(std::shared_ptr<capacity_params> p):
        capacity_t(std::move(p)) {
    initialize();
}

capacity_kibam_t::capacity_kibam_t(const capacity_kibam_t &rhs) :
        capacity_t(rhs) {
    operator=(rhs);
}

capacity_t *capacity_kibam_t::clone() {
    return new capacity_kibam_t(*this);
}

void capacity_kibam_t::replace_battery(double replacement_percent) {
    replacement_percent = fmax(0, replacement_percent);
    double qmax_old = state->qmax_lifetime;
    state->qmax_lifetime += replacement_percent * 0.01 * params->qmax_init;
    state->qmax_lifetime = fmin(state->qmax_lifetime, params->qmax_init);
    state->qmax_thermal = state->qmax_lifetime;
    state->q0 += (state->qmax_lifetime - qmax_old) * params->initial_SOC * 0.01;
    state->leadacid.q1_0 = state->q0 * c;
    state->leadacid.q2_0 = state->q0 - state->leadacid.q1_0;
    state->SOC = params->initial_SOC;
    state->SOC_prev = 50;
    update_SOC();
}

double capacity_kibam_t::c_compute(double F, double t1, double t2, double k_guess) {
    double num = F * (1 - exp(-k_guess * t1)) * t2 - (1 - exp(-k_guess * t2)) * t1;
    double denom = F * (1 - exp(-k_guess * t1)) * t2 - (1 - exp(-k_guess * t2)) * t1 - k_guess * F * t1 * t2 +
                   k_guess * t1 * t2;
    return (num / denom);
}

double capacity_kibam_t::q1_compute(double q10, double q0, double dt, double I) {
    double A = q10 * exp(-k * dt);
    double B = (q0 * k * c - I) * (1 - exp(-k * dt)) / k;
    double C = I * c * (k * dt - 1 + exp(-k * dt)) / k;
    return (A + B - C);
}

double capacity_kibam_t::q2_compute(double q20, double q0, double dt, double I) {
    double A = q20 * exp(-k * dt);
    double B = q0 * (1 - c) * (1 - exp(-k * dt));
    double C = I * (1 - c) * (k * dt - 1 + exp(-k * dt)) / k;
    return (A + B - C);
}

double capacity_kibam_t::Icmax_compute(double q10, double q0, double dt) {
    double num = -k * c * state->qmax_lifetime + k * q10 * exp(-k * dt) + q0 * k * c * (1 - exp(-k * dt));
    double denom = 1 - exp(-k * dt) + c * (k * dt - 1 + exp(-k * dt));
    return (num / denom);
}

double capacity_kibam_t::Idmax_compute(double q10, double q0, double dt) {
    double num = k * q10 * exp(-k * dt) + q0 * k * c * (1 - exp(-k * dt));
    double denom = 1 - exp(-k * dt) + c * (k * dt - 1 + exp(-k * dt));
    return (num / denom);
}

double capacity_kibam_t::qmax_compute() {
    double num = params->leadacid.q20 * ((1 - exp(-k * 20)) * (1 - c) + k * c * 20);
    double denom = k * c * 20;
    return (num / denom);
}

double capacity_kibam_t::qmax_of_i_compute(double T) {
    return ((state->qmax_lifetime * k * c * T) / (1 - exp(-k * T) + c * (k * T - 1 + exp(-k * T))));
}

void capacity_kibam_t::parameter_compute() {
    double k_guess = 0.;
    double c1 = 0.;
    double c2 = 0.;
    double minRes = 10000.;

    for (int i = 0; i < 5000; i++) {
        k_guess = i * 0.001;
        c1 = c_compute(params->leadacid.F1, params->leadacid.tn, 20, k_guess);
        c2 = c_compute(params->leadacid.F2, params->leadacid.tn, params->leadacid.t2, k_guess);

        if (fabs(c1 - c2) < minRes) {
            minRes = fabs(c1 - c2);
            k = k_guess;
            c = 0.5 * (c1 + c2);
        }
    }
    state->qmax_lifetime = qmax_compute();
}

void capacity_kibam_t::updateCapacity(double &I, double dt_hour) {
    if (fabs(I) < low_tolerance)
        I = 0;

    state->SOC_prev = state->SOC;
    state->I_loss = 0.;
    state->cell_current = I;
    params->dt_hr = dt_hour;

    double Idmax = 0.;
    double Icmax = 0.;
    double Id = 0.;
    double Ic = 0.;
    double q1 = 0.;
    double q2 = 0.;

    if (state->cell_current > 0) {
        Idmax = Idmax_compute(state->leadacid.q1_0, state->q0, dt_hour);
        Id = fmin(state->cell_current, Idmax);
        state->cell_current = Id;
    } else if (state->cell_current < 0) {
        Icmax = Icmax_compute(state->leadacid.q1_0, state->q0, dt_hour);
        Ic = -fmin(fabs(state->cell_current), fabs(Icmax));
        state->cell_current = Ic;
    }

    // new charge levels
    q1 = q1_compute(state->leadacid.q1_0, state->q0, dt_hour, state->cell_current);
    q2 = q2_compute(state->leadacid.q2_0, state->q0, dt_hour, state->cell_current);

    // Check for thermal effects
    if (q1 + q2 > state->qmax_thermal) {
        double q0 = q1 + q2;
        double p1 = q1 / q0;
        double p2 = q2 / q0;
        state->q0 = state->qmax_thermal;
        q1 = state->q0 * p1;
        q2 = state->q0 * p2;
    }

    // update internal variables
    state->leadacid.q1_0 = q1;
    state->leadacid.q2_0 = q2;
    state->q0 = q1 + q2;

    update_SOC();
    check_charge_change();

    // Pass current out
    I = state->cell_current;
}

void capacity_kibam_t::updateCapacityForThermal(double capacity_percent) {
    if (capacity_percent < 0)
        capacity_percent = 0;
    // Modify the lifetime degraded capacity by the thermal effect
    state->qmax_thermal = state->qmax_lifetime * capacity_percent * 0.01;

    // scale to q0 = qmax if q0 > qmax
    if (state->q0 > state->qmax_thermal) {
        double q0_orig = state->q0;
        double p = state->qmax_thermal / state->q0;
        state->q0 *= p;
        state->leadacid.q1 *= p;
        state->leadacid.q2 *= p;
        state->I_loss += (q0_orig - state->q0) / params->dt_hr;
    }
    update_SOC();
}

void capacity_kibam_t::updateCapacityForLifetime(double capacity_percent) {
    if (capacity_percent < 0)
        capacity_percent = 0;
    if (params->qmax_init * capacity_percent * 0.01 <= state->qmax_lifetime)
        state->qmax_lifetime = params->qmax_init * capacity_percent * 0.01;

    // scale to q0 = qmax if q0 > qmax
    if (state->q0 > state->qmax_lifetime) {
        double q0_orig = state->q0;
        double p = state->qmax_lifetime / state->q0;
        state->q0 *= p;
        state->leadacid.q1 *= p;
        state->leadacid.q2 *= p;
        state->I_loss += (q0_orig - state->q0) / params->dt_hr;
    }
    update_SOC();
}

double capacity_kibam_t::q1() { return state->leadacid.q1_0; }

double capacity_kibam_t::q2() { return state->leadacid.q2_0; }

double capacity_kibam_t::q10() { return params->leadacid.q10; }

double capacity_kibam_t::q20() { return params->leadacid.q20; }


/*
Define Lithium Ion capacity model
*/
capacity_lithium_ion_t::capacity_lithium_ion_t(double q, double SOC_init, double SOC_max, double SOC_min, double dt_hr)
        :
        capacity_t(q, SOC_init, SOC_max, SOC_min, dt_hr) {
}


capacity_lithium_ion_t::capacity_lithium_ion_t(std::shared_ptr<capacity_params> p):
        capacity_t(std::move(p)) {
}

capacity_lithium_ion_t::capacity_lithium_ion_t(const capacity_lithium_ion_t &rhs) :
        capacity_t(rhs) {}

capacity_lithium_ion_t &capacity_lithium_ion_t::operator=(const capacity_t &rhs) {
    if (this != &rhs)
        capacity_t::operator=(rhs);
    return *this;
}

capacity_t *capacity_lithium_ion_t::clone() {
    return new capacity_lithium_ion_t(*this);
}

void capacity_lithium_ion_t::replace_battery(double replacement_percent) {
    replacement_percent = fmax(0, replacement_percent);
    double qmax_old = state->qmax_lifetime;
    state->qmax_lifetime += params->qmax_init * replacement_percent * 0.01;
    state->qmax_lifetime = fmin(params->qmax_init, state->qmax_lifetime);
    state->qmax_thermal = state->qmax_lifetime;
    state->q0 += (state->qmax_lifetime - qmax_old) * params->initial_SOC * 0.01;
    state->SOC = params->initial_SOC;
    state->SOC_prev = 50;
    update_SOC();
}

void capacity_lithium_ion_t::updateCapacity(double &I, double dt) {
    state->SOC_prev = state->SOC;
    state->I_loss = 0.;
    params->dt_hr = dt;
    state->cell_current = I;

    // compute charge change ( I > 0 discharging, I < 0 charging)
    state->q0 -= state->cell_current * dt;

    // check if SOC constraints violated, update q0, I if so
    check_SOC();

    // update SOC, DOD
    update_SOC();
    check_charge_change();

    // Pass current out
    I = state->cell_current;
}

void capacity_lithium_ion_t::updateCapacityForThermal(double capacity_percent) {
    if (capacity_percent < 0)
        capacity_percent = 0;
    // Modify the lifetime degraded capacity by the thermal effect
    state->qmax_thermal = state->qmax_lifetime * capacity_percent * 0.01;
    if (state->q0 > state->qmax_thermal) {
        state->I_loss += (state->q0 - state->qmax_thermal) / params->dt_hr;
        state->q0 = state->qmax_thermal;
    }
    update_SOC();
}

void capacity_lithium_ion_t::updateCapacityForLifetime(double capacity_percent) {
    if (capacity_percent < 0)
        capacity_percent = 0;
    if (params->qmax_init * capacity_percent * 0.01 <= state->qmax_lifetime)
        state->qmax_lifetime = params->qmax_init * capacity_percent * 0.01;

    if (state->q0 > state->qmax_lifetime) {
        state->I_loss += (state->q0 - state->qmax_lifetime) / params->dt_hr;
        state->q0 = state->qmax_lifetime;
    }

    update_SOC();
}

double capacity_lithium_ion_t::q1() { return state->q0; }

double capacity_lithium_ion_t::q10() { return state->qmax_lifetime; }
