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
#include <memory>

#include "lib_battery_lifetime.h"
#include "lib_battery_lifetime_nmc.h"
#include "lib_battery_lifetime_calendar_cycle.h"

extern double tolerance;
extern double low_tolerance;

void lifetime_cycle_t::initialize() {
    state->n_cycles = 0;
    state->range = 0;
    state->average_range = 0;
    state->cycle->q_relative_cycle = bilinear(0., 0);
    state->cycle->rainflow_jlt = 0;
    state->cycle->rainflow_Xlt = 0;
    state->cycle->rainflow_Ylt = 0;
    state->cycle->rainflow_peaks.clear();
}

lifetime_cycle_t::lifetime_cycle_t(const util::matrix_t<double> &batt_lifetime_matrix) {
    params = std::make_shared<lifetime_params>();
    params->cal_cyc->cycling_matrix = batt_lifetime_matrix;
    state = std::make_shared<lifetime_state>();
    initialize();
}

lifetime_cycle_t::lifetime_cycle_t(std::shared_ptr<lifetime_params> params_ptr) :
        params(std::move(params_ptr)) {
    state = std::make_shared<lifetime_state>();
    initialize();
}

lifetime_cycle_t::lifetime_cycle_t(std::shared_ptr<lifetime_params> params_ptr, std::shared_ptr<lifetime_state> state_ptr) :
        params(std::move(params_ptr)),
        state(std::move(state_ptr)){
}

lifetime_cycle_t::lifetime_cycle_t(const lifetime_cycle_t &rhs) {
    state = std::make_shared<lifetime_state>(*rhs.state);
    operator=(rhs);
}

lifetime_cycle_t &lifetime_cycle_t::operator=(const lifetime_cycle_t &rhs) {
    if (this != &rhs) {
        *state = *rhs.state;
        *params = *rhs.params;
    }
    return *this;
}

lifetime_cycle_t *lifetime_cycle_t::clone() {
    return new lifetime_cycle_t(*this);
}

double lifetime_cycle_t::estimateCycleDamage() {
    // Initialize assuming 50% DOD
    double DOD = 50;
    if (state->average_range > 0) {
        DOD = state->average_range;
    }
    return (bilinear(DOD, state->n_cycles + 1) - bilinear(DOD, state->n_cycles + 2));
}

double lifetime_cycle_t::runCycleLifetime(double DOD) {
    rainflow(DOD);

    // return the effective capacity (Q_neg)
    return state->cycle->q_relative_cycle;
}

void lifetime_cycle_t::rainflow(double DOD) {
    // initialize return code
    int retCode = cycle_state::LT_GET_DATA;

    // Begin algorithm
    state->cycle->rainflow_peaks.push_back(DOD);
    bool atStepTwo = true;

    // Loop until break
    while (atStepTwo) {
        // Rainflow: Step 2: Form ranges X,Y
        if (state->cycle->rainflow_jlt >= 2)
            rainflow_ranges();
        else {
            // Get more data (Step 1)
            retCode = cycle_state::LT_GET_DATA;
            break;
        }

        // Rainflow: Step 3: Compare ranges
        retCode = rainflow_compareRanges();

        // We break to get more data, or if we are done with step 5
        if (retCode == cycle_state::LT_GET_DATA)
            break;
    }

    if (retCode == cycle_state::LT_GET_DATA)
        state->cycle->rainflow_jlt++;
}

void lifetime_cycle_t::rainflow_ranges() {
    state->cycle->rainflow_Ylt = fabs(state->cycle->rainflow_peaks[state->cycle->rainflow_jlt - (size_t) 1] - state->cycle->rainflow_peaks[state->cycle->rainflow_jlt - (size_t) 2]);
    state->cycle->rainflow_Xlt = fabs(state->cycle->rainflow_peaks[state->cycle->rainflow_jlt] - state->cycle->rainflow_peaks[state->cycle->rainflow_jlt - (size_t) 1]);
}

void lifetime_cycle_t::rainflow_ranges_circular(int index) {
    size_t end = state->cycle->rainflow_peaks.size() - 1;
    if (index == 0) {
        state->cycle->rainflow_Xlt = fabs(state->cycle->rainflow_peaks[0] - state->cycle->rainflow_peaks[end]);
        state->cycle->rainflow_Ylt = fabs(state->cycle->rainflow_peaks[end] - state->cycle->rainflow_peaks[end - 1]);
    } else if (index == 1) {
        state->cycle->rainflow_Xlt = fabs(state->cycle->rainflow_peaks[1] - state->cycle->rainflow_peaks[0]);
        state->cycle->rainflow_Ylt = fabs(state->cycle->rainflow_peaks[0] - state->cycle->rainflow_peaks[end]);
    } else
        rainflow_ranges();
}

int lifetime_cycle_t::rainflow_compareRanges() {
    int retCode = cycle_state::LT_SUCCESS;
    bool contained = true;

    // modified to disregard some of algorithm which doesn't work well
    if (state->cycle->rainflow_Xlt + tolerance < state->cycle->rainflow_Ylt)
        retCode = cycle_state::LT_GET_DATA;
    else
        contained = false;

    // Step 5: Count range Y, discard peak & valley of Y, go to Step 2
    if (!contained) {
        state->range = state->cycle->rainflow_Ylt;
        state->average_range = (state->average_range * state->n_cycles + state->range) / (double)(state->n_cycles + (size_t) 1);
        state->n_cycles++;

        // the capacity percent cannot increase
        double dq =
                bilinear(state->average_range, state->n_cycles) - bilinear(state->average_range, state->n_cycles + 1);
        if (dq > 0)
            state->cycle->q_relative_cycle -= dq;

        if (state->cycle->q_relative_cycle < 0)
            state->cycle->q_relative_cycle = 0.;

        // discard peak & valley of Y
        double save = state->cycle->rainflow_peaks[state->cycle->rainflow_jlt];
        state->cycle->rainflow_peaks.pop_back();
        state->cycle->rainflow_peaks.pop_back();
        state->cycle->rainflow_peaks.pop_back();
        state->cycle->rainflow_peaks.push_back(save);
        state->cycle->rainflow_jlt -= 2;
        // stay in while loop
        retCode = cycle_state::LT_RERANGE;
    }

    return retCode;
}

void lifetime_cycle_t::replaceBattery(double replacement_percent) {
    state->cycle->q_relative_cycle += replacement_percent;
    state->cycle->q_relative_cycle = fmin(bilinear(0., 0), state->cycle->q_relative_cycle);

    // More work to figure out degradation of multiple-aged battery units
    if (replacement_percent == 100) {
        state->n_cycles = 0;
    }

    state->cycle->rainflow_jlt = 0;
    state->cycle->rainflow_Xlt = 0;
    state->cycle->rainflow_Ylt = 0;
    state->range = 0;
    state->cycle->rainflow_peaks.clear();
}

int lifetime_cycle_t::cycles_elapsed() { return state->n_cycles; }

double lifetime_cycle_t::cycle_range() { return state->range; }

double lifetime_cycle_t::average_range() { return state->average_range; }

double lifetime_cycle_t::capacity_percent() { return state->cycle->q_relative_cycle; }

lifetime_state lifetime_cycle_t::get_state() { return *state; }

double lifetime_cycle_t::bilinear(double DOD, int cycle_number) {
    /*
    Work could be done to make this simpler
    Current idea is to interpolate first along the C = f(n) curves for each DOD to get C_DOD_, C_DOD_+
    Then interpolate C_, C+ to get C at the DOD of interest
    */

    std::vector<double> D_unique_vect;
    std::vector<double> C_n_low_vect;
    std::vector<double> D_high_vect;
    std::vector<double> C_n_high_vect;
    std::vector<size_t> low_indices;
    std::vector<size_t> high_indices;
    double D = 0.;
    size_t n = 0;
    double C = 100;
    size_t n_rows = params->cal_cyc->cycling_matrix.nrows();

    // get unique values of D
    D_unique_vect.push_back(params->cal_cyc->cycling_matrix.at(0, calendar_cycle_params::DOD));
    for (size_t i = 0; i < n_rows; i++) {
        bool contained = false;
        for (double j : D_unique_vect) {
            if (params->cal_cyc->cycling_matrix.at(i, calendar_cycle_params::DOD) == j) {
                contained = true;
                break;
            }
        }
        if (!contained) {
            D_unique_vect.push_back(params->cal_cyc->cycling_matrix.at(i, calendar_cycle_params::DOD));
        }
    }
    n = D_unique_vect.size();

    if (n > 1) {
        // get where DOD is bracketed [D_lo, DOD, D_hi]
        double D_lo = 0;
        double D_hi = 100;

        for (size_t i = 0; i < n_rows; i++) {
            D = params->cal_cyc->cycling_matrix.at(i, calendar_cycle_params::DOD);
            if (D < DOD && D > D_lo)
                D_lo = D;
            else if (D >= DOD && D < D_hi)
                D_hi = D;
        }

        // Separate table into bins
        double D_min = 100.;
        double D_max = 0.;

        for (size_t i = 0; i < n_rows; i++) {
            D = params->cal_cyc->cycling_matrix.at(i, calendar_cycle_params::DOD);
            if (D == D_lo)
                low_indices.push_back(i);
            else if (D == D_hi)
                high_indices.push_back(i);

            if (D < D_min) { D_min = D; }
            else if (D > D_max) { D_max = D; }
        }

        // if we're out of the bounds, just make the upper bound equal to the highest input
        if (high_indices.empty()) {
            for (size_t i = 0; i != n_rows; i++) {
                if (params->cal_cyc->cycling_matrix.at(i, calendar_cycle_params::DOD) == D_max)
                    high_indices.push_back(i);
            }
        }

        size_t n_rows_lo = low_indices.size();
        size_t n_rows_hi = high_indices.size();
        size_t n_cols = 2;

        // If we aren't bounded, fill in values
        if (n_rows_lo == 0) {
            // Assumes 0% DOD
            for (int i = 0; i < n_rows_hi; i++) {
                C_n_low_vect.push_back(0. + (double)i * 500); // cycles
                C_n_low_vect.push_back(100.); // 100 % capacity
            }
        }

        if (n_rows_lo != 0) {
            for (int i = 0; i < (int) n_rows_lo; i++) {
                C_n_low_vect.push_back(params->cal_cyc->cycling_matrix.at(low_indices[i], calendar_cycle_params::CYCLE));
                C_n_low_vect.push_back(params->cal_cyc->cycling_matrix.at(low_indices[i], calendar_cycle_params::CAPACITY_CYCLE));
            }
        }
        if (n_rows_hi != 0) {
            for (int i = 0; i < (int) n_rows_hi; i++) {
                C_n_high_vect.push_back(params->cal_cyc->cycling_matrix.at(high_indices[i], calendar_cycle_params::CYCLE));
                C_n_high_vect.push_back(params->cal_cyc->cycling_matrix.at(high_indices[i], calendar_cycle_params::CAPACITY_CYCLE));
            }
        }
        n_rows_lo = C_n_low_vect.size() / n_cols;
        n_rows_hi = C_n_high_vect.size() / n_cols;

        if (n_rows_lo == 0 || n_rows_hi == 0) {
            // need a safeguard here
        }

        util::matrix_t<double> C_n_low(n_rows_lo, n_cols, &C_n_low_vect);
        util::matrix_t<double> C_n_high(n_rows_lo, n_cols, &C_n_high_vect);

        // Compute C(D_lo, n), C(D_hi, n)
        double C_Dlo = util::linterp_col(C_n_low, 0, cycle_number, 1);
        double C_Dhi = util::linterp_col(C_n_high, 0, cycle_number, 1);

        if (C_Dlo < 0.)
            C_Dlo = 0.;
        if (C_Dhi > 100.)
            C_Dhi = 100.;

        // Interpolate to get C(D, n)
        C = util::interpolate(D_lo, C_Dlo, D_hi, C_Dhi, DOD);
    }
        // just have one row, single level interpolation
    else {
        C = util::linterp_col(params->cal_cyc->cycling_matrix, 1, cycle_number, 2);
    }

    return C;
}

/*
Lifetime Calendar Model
*/

bool calendar_state::operator==(const calendar_state &p) const {
    bool equal = (q_relative_calendar == p.q_relative_calendar);
//    equal &= (day_age_of_battery == p.day_age_of_battery);
    equal &= (dq_relative_calendar_old == p.dq_relative_calendar_old);
    return equal;
}

void lifetime_calendar_t::initialize() {
    state->day_age_of_battery = 0;
    state->calendar->q_relative_calendar = 100;
    state->calendar->dq_relative_calendar_old = 0;
    if (params->cal_cyc->calendar_choice == calendar_cycle_params::CALENDAR_CHOICE::MODEL) {
        dt_day = params->dt_hr / util::hours_per_day;
        state->calendar->q_relative_calendar = params->cal_cyc->calendar_q0 * 100;
    }
    else if (params->cal_cyc->calendar_choice == calendar_cycle_params::CALENDAR_CHOICE::TABLE) {
        if (params->cal_cyc->calendar_matrix.nrows() < 2 || params->cal_cyc->calendar_matrix.ncols() != 2)
            throw std::runtime_error("lifetime_calendar_t error: Battery calendar lifetime matrix must have 2 columns and at least 2 rows");
    }
}

lifetime_calendar_t::lifetime_calendar_t(double dt_hour, const util::matrix_t<double>& calendar_matrix) {
    params = std::make_shared<lifetime_params>();
    params->dt_hr = dt_hour;
    params->cal_cyc->calendar_choice = calendar_cycle_params::CALENDAR_CHOICE::TABLE;
    params->cal_cyc->calendar_matrix = calendar_matrix;
    state = std::make_shared<lifetime_state>();
    initialize();
}


lifetime_calendar_t::lifetime_calendar_t(double dt_hour, double q0, double a, double b, double c) {
    params = std::make_shared<lifetime_params>();
    params->dt_hr = dt_hour;
    params->cal_cyc->calendar_choice = calendar_cycle_params::CALENDAR_CHOICE::MODEL;
    params->cal_cyc->calendar_q0 = q0;
    params->cal_cyc->calendar_a = a;
    params->cal_cyc->calendar_b = b;
    params->cal_cyc->calendar_c = c;
    state = std::make_shared<lifetime_state>();
    initialize();
}

lifetime_calendar_t::lifetime_calendar_t(std::shared_ptr<lifetime_params> params_ptr, std::shared_ptr<lifetime_state> state_ptr) :
        params(std::move(params_ptr)),
        state(std::move(state_ptr))
{
}

lifetime_calendar_t::lifetime_calendar_t(const lifetime_calendar_t &rhs) {
    state = std::make_shared<lifetime_state>(*rhs.state);
    params = std::make_shared<lifetime_params>(*rhs.params);
    dt_day = rhs.dt_day;
}

lifetime_calendar_t &lifetime_calendar_t::operator=(const lifetime_calendar_t &rhs) {
    if (this != &rhs) {
        *params = *rhs.params;
        *state = *rhs.state;
        dt_day = rhs.dt_day;
    }
    return *this;
}

lifetime_calendar_t *lifetime_calendar_t::clone() {
    return new lifetime_calendar_t(*this);
}

double lifetime_calendar_t::capacity_percent() { return state->calendar->q_relative_calendar; }

lifetime_state lifetime_calendar_t::get_state() { return *state; }

double lifetime_calendar_t::runLifetimeCalendarModel(size_t lifetimeIndex, double T, double SOC) {
    state->day_age_of_battery = lifetimeIndex / (util::hours_per_day / params->dt_hr);

    if (params->cal_cyc->calendar_choice == calendar_cycle_params::CALENDAR_CHOICE::MODEL)
        runLithiumIonModel(T, SOC);
    else if (params->cal_cyc->calendar_choice == calendar_cycle_params::CALENDAR_CHOICE::TABLE)
        runTableModel();
    else
        state->calendar->q_relative_calendar = 100;

    return state->calendar->q_relative_calendar;
}

void lifetime_calendar_t::runLithiumIonModel(double temp, double SOC) {
    temp += 273.15;
    SOC *= 0.01;
    double k_cal = params->cal_cyc->calendar_a * exp(params->cal_cyc->calendar_b * (1. / temp - 1. / 296))
                   * exp(params->cal_cyc->calendar_c * (SOC / temp - 1. / 296));
    double dq_new;
    if (state->calendar->dq_relative_calendar_old == 0)
        dq_new = k_cal * sqrt(dt_day);
    else
        dq_new = (0.5 * pow(k_cal, 2) / state->calendar->dq_relative_calendar_old) * dt_day + state->calendar->dq_relative_calendar_old;
    state->calendar->dq_relative_calendar_old = dq_new;
    state->calendar->q_relative_calendar = (params->cal_cyc->calendar_q0 - (dq_new)) * 100;
}

void lifetime_calendar_t::runTableModel() {
    size_t n_rows = params->cal_cyc->calendar_matrix.nrows();
    size_t n = n_rows - 1;
    size_t day_lo = 0;
    auto day_hi = (size_t) params->cal_cyc->calendar_matrix.at(n, calendar_cycle_params::DAYS);
    double capacity_lo = 100;
    double capacity_hi = 0;

    // interpolation mode
    for (size_t i = 0; i != n_rows; i++) {
        int day = (int)params->cal_cyc->calendar_matrix.at(i, calendar_cycle_params::DAYS);
        double capacity = (int) params->cal_cyc->calendar_matrix.at(i, calendar_cycle_params::CAPACITY_CAL);
        if (day <= (int)state->day_age_of_battery) {
            day_lo = day;
            capacity_lo = capacity;
        }
        if (day > (int)state->day_age_of_battery) {
            day_hi = day;
            capacity_hi = capacity;
            break;
        }
    }
    if (day_lo == day_hi) {
        day_lo = (int) params->cal_cyc->calendar_matrix.at(n - 1, calendar_cycle_params::DAYS);
        day_hi = (int) params->cal_cyc->calendar_matrix.at(n, calendar_cycle_params::DAYS);
        capacity_lo = (int) params->cal_cyc->calendar_matrix.at(n - 1, calendar_cycle_params::CAPACITY_CAL);
        capacity_hi = (int) params->cal_cyc->calendar_matrix.at(n, calendar_cycle_params::CAPACITY_CAL);
    }

    state->calendar->q_relative_calendar = util::interpolate((double) day_lo, capacity_lo, (double) day_hi, capacity_hi, state->day_age_of_battery);
}

void lifetime_calendar_t::replaceBattery(double replacement_percent) {
    state->day_age_of_battery = 0;
    state->calendar->dq_relative_calendar_old = 0;
    state->calendar->q_relative_calendar += replacement_percent;
    if (params->cal_cyc->calendar_choice == calendar_cycle_params::MODEL)
        state->calendar->q_relative_calendar = fmin(params->cal_cyc->calendar_q0 * 100, state->calendar->q_relative_calendar);
    if (params->cal_cyc->calendar_choice == calendar_cycle_params::TABLE)
        state->calendar->q_relative_calendar = fmin(100, state->calendar->q_relative_calendar);
}

/*
Define Lifetime Model
*/

void lifetime_calendar_cycle_t::initialize() {
    state = std::make_shared<lifetime_state>();
    if (params->cal_cyc->cycling_matrix.nrows() < 3 || params->cal_cyc->cycling_matrix.ncols() != 3)
        throw std::runtime_error("lifetime_cycle_t error: Battery lifetime matrix must have three columns and at least three rows");
    cycle_model = std::unique_ptr<lifetime_cycle_t>(new lifetime_cycle_t(params, state));
    cycle_model->initialize();
    calendar_model = std::unique_ptr<lifetime_calendar_t>(new lifetime_calendar_t(params, state));
    calendar_model->initialize();
    state->q_relative = fmin(state->cycle->q_relative_cycle, state->calendar->q_relative_calendar);
}

lifetime_calendar_cycle_t::lifetime_calendar_cycle_t(const util::matrix_t<double> &batt_lifetime_matrix, double dt_hour,
                                                     const util::matrix_t<double> &calendar_matrix) {
    params = std::make_shared<lifetime_params>();
    params->model_choice = lifetime_params::CALCYC;
    params->dt_hr = dt_hour;
    params->cal_cyc->cycling_matrix = batt_lifetime_matrix;
    params->cal_cyc->calendar_choice = calendar_cycle_params::CALENDAR_CHOICE::TABLE;
    params->cal_cyc->calendar_matrix = calendar_matrix;

    initialize();
}

lifetime_calendar_cycle_t::lifetime_calendar_cycle_t(const util::matrix_t<double> &batt_lifetime_matrix, double dt_hour, double q0, double a, double b,
                                                     double c) {
    params = std::make_shared<lifetime_params>();
    params->model_choice = lifetime_params::CALCYC;
    params->dt_hr = dt_hour;
    params->cal_cyc->cycling_matrix = batt_lifetime_matrix;
    params->cal_cyc->calendar_choice = calendar_cycle_params::CALENDAR_CHOICE::MODEL;
    params->cal_cyc->calendar_q0 = q0;
    params->cal_cyc->calendar_a = a;
    params->cal_cyc->calendar_b = b;
    params->cal_cyc->calendar_c = c;

    initialize();
}

lifetime_calendar_cycle_t::lifetime_calendar_cycle_t(const util::matrix_t<double> &batt_lifetime_matrix, double dt_hour) {
    params = std::make_shared<lifetime_params>();
    params->model_choice = lifetime_params::CALCYC;
    params->dt_hr = dt_hour;
    params->cal_cyc->cycling_matrix = batt_lifetime_matrix;
    params->cal_cyc->calendar_choice = calendar_cycle_params::CALENDAR_CHOICE::NONE;

    initialize();
}

lifetime_calendar_cycle_t::lifetime_calendar_cycle_t(std::shared_ptr<lifetime_params> params_ptr) {
    params = std::move(params_ptr);
    initialize();
}

lifetime_calendar_cycle_t::lifetime_calendar_cycle_t(const lifetime_calendar_cycle_t& rhs) :
        lifetime_t(rhs){
    operator=(rhs);
}

lifetime_calendar_cycle_t& lifetime_calendar_cycle_t::operator=(const lifetime_calendar_cycle_t& rhs) {
    if (this != &rhs) {
        *params = *rhs.params;
        *state = *rhs.state;
        calendar_model = std::unique_ptr<lifetime_calendar_t>(new lifetime_calendar_t(params, state));
        cycle_model = std::unique_ptr<lifetime_cycle_t>(new lifetime_cycle_t(params, state));
    }
    return *this;
}

lifetime_calendar_cycle_t *lifetime_calendar_cycle_t::clone() {
    return new lifetime_calendar_cycle_t(*this);
}

double lifetime_calendar_cycle_t::capacity_percent_cycle() { return cycle_model->capacity_percent(); }

double lifetime_calendar_cycle_t::capacity_percent_calendar() { return calendar_model->capacity_percent(); }

void lifetime_calendar_cycle_t::runLifetimeModels(size_t lifetimeIndex, bool charge_changed, double prev_DOD, double DOD, double T_battery) {
    double q_last = state->q_relative;

    if (q_last > 0) {
        double q_cycle = cycle_model->capacity_percent();
        double q_calendar;

        if (charge_changed)
            q_cycle = cycle_model->runCycleLifetime(prev_DOD);
        else if (lifetimeIndex == 0)
            q_cycle = cycle_model->runCycleLifetime(DOD);

        q_calendar = calendar_model->runLifetimeCalendarModel(lifetimeIndex, T_battery, 100. - DOD);

        // total capacity is min of cycle (Q_neg) and calendar (Q_li) capacity
        state->q_relative = fmin(q_cycle, q_calendar);
    }
    state->q_relative = fmax(state->q_relative, 0);

    // capacity cannot increase
    state->q_relative = fmin(state->q_relative, q_last);
}

double lifetime_calendar_cycle_t::estimateCycleDamage() {
    return cycle_model->estimateCycleDamage();
}

void lifetime_calendar_cycle_t::replaceBattery(double percent_to_replace) {
    cycle_model->replaceBattery(percent_to_replace);
    calendar_model->replaceBattery(percent_to_replace);
    state->q_relative = fmin(cycle_model->capacity_percent(), calendar_model->capacity_percent());
}

