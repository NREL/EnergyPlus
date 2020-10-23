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

#include "lib_battery_lifetime.h"

extern double tolerance;
extern double low_tolerance;

bool cycle_state::operator==(const cycle_state &p) {
    bool equal = (q_relative_cycle == p.q_relative_cycle);
    equal &= (n_cycles == p.n_cycles);
    equal &= (range == p.range);
    equal &= (average_range == p.average_range);
    equal &= (rainflow_Xlt == p.rainflow_Xlt);
    equal &= (rainflow_Ylt == p.rainflow_Ylt);
    equal &= (rainflow_jlt == p.rainflow_jlt);
    equal &= (rainflow_peaks == p.rainflow_peaks);
    return equal;
}

void lifetime_cycle_t::initialize() {
    if (params->cycling_matrix.nrows() < 3 || params->cycling_matrix.ncols() != 3)
        throw std::runtime_error("lifetime_cycle_t error: Battery lifetime matrix must have three columns and at least three rows");
    state = std::make_shared<cycle_state>();
    state->n_cycles = 0;
    state->q_relative_cycle = bilinear(0., 0);
    state->range = 0;
    state->average_range = 0;
    state->rainflow_jlt = 0;
    state->rainflow_Xlt = 0;
    state->rainflow_Ylt = 0;
    state->rainflow_peaks.clear();
}

lifetime_cycle_t::lifetime_cycle_t(const util::matrix_t<double> &batt_lifetime_matrix) {
    params = std::make_shared<lifetime_params>();
    params->cycling_matrix = batt_lifetime_matrix;
    initialize();
}

lifetime_cycle_t::lifetime_cycle_t(std::shared_ptr<lifetime_params> params_ptr) :
        params(std::move(params_ptr)) {
    initialize();
}

lifetime_cycle_t::lifetime_cycle_t(const lifetime_cycle_t &rhs) {
    state = std::make_shared<cycle_state>(*rhs.state);
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
    return state->q_relative_cycle;
}

void lifetime_cycle_t::rainflow(double DOD) {
    // initialize return code
    int retCode = cycle_state::LT_GET_DATA;

    // Begin algorithm
    state->rainflow_peaks.push_back(DOD);
    bool atStepTwo = true;

    // Loop until break
    while (atStepTwo) {
        // Rainflow: Step 2: Form ranges X,Y
        if (state->rainflow_jlt >= 2)
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
        state->rainflow_jlt++;
}

void lifetime_cycle_t::rainflow_ranges() {
    state->rainflow_Ylt = fabs(state->rainflow_peaks[state->rainflow_jlt - 1] - state->rainflow_peaks[state->rainflow_jlt - 2]);
    state->rainflow_Xlt = fabs(state->rainflow_peaks[state->rainflow_jlt] - state->rainflow_peaks[state->rainflow_jlt - 1]);
}

void lifetime_cycle_t::rainflow_ranges_circular(int index) {
    size_t end = state->rainflow_peaks.size() - 1;
    if (index == 0) {
        state->rainflow_Xlt = fabs(state->rainflow_peaks[0] - state->rainflow_peaks[end]);
        state->rainflow_Ylt = fabs(state->rainflow_peaks[end] - state->rainflow_peaks[end - 1]);
    } else if (index == 1) {
        state->rainflow_Xlt = fabs(state->rainflow_peaks[1] - state->rainflow_peaks[0]);
        state->rainflow_Ylt = fabs(state->rainflow_peaks[0] - state->rainflow_peaks[end]);
    } else
        rainflow_ranges();
}

int lifetime_cycle_t::rainflow_compareRanges() {
    int retCode = cycle_state::LT_SUCCESS;
    bool contained = true;

    // modified to disregard some of algorithm which doesn't work well
    if (state->rainflow_Xlt < state->rainflow_Ylt)
        retCode = cycle_state::LT_GET_DATA;
    else if (state->rainflow_Xlt >= state->rainflow_Ylt)
        contained = false;

    // Step 5: Count range Y, discard peak & valley of Y, go to Step 2
    if (!contained) {
        state->range = state->rainflow_Ylt;
        state->average_range = (state->average_range * state->n_cycles + state->range) / (state->n_cycles + 1);
        state->n_cycles++;

        // the capacity percent cannot increase
        double dq =
                bilinear(state->average_range, state->n_cycles) - bilinear(state->average_range, state->n_cycles + 1);
        if (dq > 0)
            state->q_relative_cycle -= dq;

        if (state->q_relative_cycle < 0)
            state->q_relative_cycle = 0.;

        // discard peak & valley of Y
        double save = state->rainflow_peaks[state->rainflow_jlt];
        state->rainflow_peaks.pop_back();
        state->rainflow_peaks.pop_back();
        state->rainflow_peaks.pop_back();
        state->rainflow_peaks.push_back(save);
        state->rainflow_jlt -= 2;
        // stay in while loop
        retCode = cycle_state::LT_RERANGE;
    }

    return retCode;
}

void lifetime_cycle_t::replaceBattery(double replacement_percent) {
    state->q_relative_cycle += replacement_percent;
    state->q_relative_cycle = fmin(bilinear(0., 0), state->q_relative_cycle);

    // More work to figure out degradation of multiple-aged battery units
    if (replacement_percent == 100) {
        state->n_cycles = 0;
    }

    state->rainflow_jlt = 0;
    state->rainflow_Xlt = 0;
    state->rainflow_Ylt = 0;
    state->range = 0;
    state->rainflow_peaks.clear();
}

int lifetime_cycle_t::cycles_elapsed() { return state->n_cycles; }

double lifetime_cycle_t::cycle_range() { return state->range; }

double lifetime_cycle_t::average_range() { return state->average_range; }

double lifetime_cycle_t::capacity_percent() { return state->q_relative_cycle; }

cycle_state lifetime_cycle_t::get_state() { return *state; }

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
    std::vector<int> low_indices;
    std::vector<int> high_indices;
    double D = 0.;
    size_t n = 0;
    double C = 100;
    int n_rows = params->cycling_matrix.nrows();

    // get unique values of D
    D_unique_vect.push_back(params->cycling_matrix.at(0, lifetime_params::DOD));
    for (int i = 0; i < n_rows; i++) {
        bool contained = false;
        for (int j = 0; j < (int) D_unique_vect.size(); j++) {
            if (params->cycling_matrix.at(i, lifetime_params::DOD) == D_unique_vect[j]) {
                contained = true;
                break;
            }
        }
        if (!contained) {
            D_unique_vect.push_back(params->cycling_matrix.at(i, lifetime_params::DOD));
        }
    }
    n = D_unique_vect.size();

    if (n > 1) {
        // get where DOD is bracketed [D_lo, DOD, D_hi]
        double D_lo = 0;
        double D_hi = 100;

        for (int i = 0; i < n_rows; i++) {
            D = params->cycling_matrix.at(i, lifetime_params::DOD);
            if (D < DOD && D > D_lo)
                D_lo = D;
            else if (D >= DOD && D < D_hi)
                D_hi = D;
        }

        // Separate table into bins
        double D_min = 100.;
        double D_max = 0.;

        for (int i = 0; i < n_rows; i++) {
            D = params->cycling_matrix.at(i, lifetime_params::DOD);
            if (D == D_lo)
                low_indices.push_back(i);
            else if (D == D_hi)
                high_indices.push_back(i);

            if (D < D_min) { D_min = D; }
            else if (D > D_max) { D_max = D; }
        }

        // if we're out of the bounds, just make the upper bound equal to the highest input
        if (high_indices.empty()) {
            for (int i = 0; i != n_rows; i++) {
                if (params->cycling_matrix.at(i, lifetime_params::DOD) == D_max)
                    high_indices.push_back(i);
            }
        }

        size_t n_rows_lo = low_indices.size();
        size_t n_rows_hi = high_indices.size();
        size_t n_cols = 2;

        // If we aren't bounded, fill in values
        if (n_rows_lo == 0) {
            // Assumes 0% DOD
            for (int i = 0; i < (int) n_rows_hi; i++) {
                C_n_low_vect.push_back(0. + i * 500); // cycles
                C_n_low_vect.push_back(100.); // 100 % capacity
            }
        }

        if (n_rows_lo != 0) {
            for (int i = 0; i < (int) n_rows_lo; i++) {
                C_n_low_vect.push_back(params->cycling_matrix.at(low_indices[i], lifetime_params::CYCLE));
                C_n_low_vect.push_back(params->cycling_matrix.at(low_indices[i], lifetime_params::CAPACITY_CYCLE));
            }
        }
        if (n_rows_hi != 0) {
            for (int i = 0; i < (int) n_rows_hi; i++) {
                C_n_high_vect.push_back(params->cycling_matrix.at(high_indices[i], lifetime_params::CYCLE));
                C_n_high_vect.push_back(params->cycling_matrix.at(high_indices[i], lifetime_params::CAPACITY_CYCLE));
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
        C = util::linterp_col(params->cycling_matrix, 1, cycle_number, 2);
    }

    return C;
}

/*
Lifetime Calendar Model
*/

bool calendar_state::operator==(const calendar_state &p) {
    bool equal = (q_relative_calendar == p.q_relative_calendar);
    equal &= (day_age_of_battery == p.day_age_of_battery);
    equal &= (dq_relative_calendar_old == p.dq_relative_calendar_old);
    return equal;
}

void lifetime_calendar_t::initialize() {
    state = std::make_shared<calendar_state>();
    state->day_age_of_battery = 0;
    state->q_relative_calendar = 100;
    state->dq_relative_calendar_old = 0;
    if (params->calendar_choice == lifetime_params::CALENDAR_CHOICE::MODEL) {
        dt_day = params->dt_hour / util::hours_per_day;
        state->q_relative_calendar = params->calendar_q0 * 100;
    }
    else if (params->calendar_choice == lifetime_params::CALENDAR_CHOICE::TABLE) {
        if (params->calendar_matrix.nrows() < 2 || params->calendar_matrix.ncols() != 2)
            throw std::runtime_error("lifetime_calendar_t error: Battery calendar lifetime matrix must have 2 columns and at least 2 rows");
    }
}

lifetime_calendar_t::lifetime_calendar_t(double dt_hour, const util::matrix_t<double>& calendar_matrix) {
    params = std::make_shared<lifetime_params>();
    params->dt_hour = dt_hour;
    params->calendar_choice = lifetime_params::CALENDAR_CHOICE::TABLE;
    params->calendar_matrix = calendar_matrix;

    initialize();
}


lifetime_calendar_t::lifetime_calendar_t(double dt_hour, double q0, double a, double b, double c) {
    params = std::make_shared<lifetime_params>();
    params->dt_hour = dt_hour;
    params->calendar_choice = lifetime_params::CALENDAR_CHOICE::MODEL;
    params->calendar_q0 = q0;
    params->calendar_a = a;
    params->calendar_b = b;
    params->calendar_c = c;

    initialize();
}

lifetime_calendar_t::lifetime_calendar_t(std::shared_ptr<lifetime_params> params_ptr) :
        params(std::move(params_ptr)) {
    initialize();
}

lifetime_calendar_t::lifetime_calendar_t(const lifetime_calendar_t &rhs) {
    state = std::make_shared<calendar_state>(*rhs.state);
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

double lifetime_calendar_t::capacity_percent() { return state->q_relative_calendar; }

calendar_state lifetime_calendar_t::get_state() { return *state; }

double lifetime_calendar_t::runLifetimeCalendarModel(size_t lifetimeIndex, double T, double SOC) {
    state->day_age_of_battery = (size_t)(lifetimeIndex / (util::hours_per_day / params->dt_hour));

    if (params->calendar_choice == lifetime_params::CALENDAR_CHOICE::MODEL)
        runLithiumIonModel(T, SOC);
    else if (params->calendar_choice == lifetime_params::CALENDAR_CHOICE::TABLE)
        runTableModel();

    return state->q_relative_calendar;
}

void lifetime_calendar_t::runLithiumIonModel(double temp, double SOC) {
    temp += 273.15;
    SOC *= 0.01;
    double k_cal = params->calendar_a * exp(params->calendar_b * (1. / temp - 1. / 296))
                   * exp(params->calendar_c * (SOC / temp - 1. / 296));
    double dq_new;
    if (state->dq_relative_calendar_old == 0)
        dq_new = k_cal * sqrt(dt_day);
    else
        dq_new = (0.5 * pow(k_cal, 2) / state->dq_relative_calendar_old) * dt_day + state->dq_relative_calendar_old;
    state->dq_relative_calendar_old = dq_new;
    state->q_relative_calendar = (params->calendar_q0 - (dq_new)) * 100;
}

void lifetime_calendar_t::runTableModel() {
    size_t n_rows = params->calendar_matrix.nrows();
    size_t n = n_rows - 1;
    int day_lo = 0;
    int day_hi = (int) params->calendar_matrix.at(n, lifetime_params::DAYS);
    double capacity_lo = 100;
    double capacity_hi = 0;

    // interpolation mode
    for (int i = 0; i != (int) n_rows; i++) {
        int day = (int) params->calendar_matrix.at(i, lifetime_params::DAYS);
        double capacity = (int) params->calendar_matrix.at(i, lifetime_params::CAPACITY_CAL);
        if (day <= state->day_age_of_battery) {
            day_lo = day;
            capacity_lo = capacity;
        }
        if (day > state->day_age_of_battery) {
            day_hi = day;
            capacity_hi = capacity;
            break;
        }
    }
    if (day_lo == day_hi) {
        day_lo = (int) params->calendar_matrix.at(n - 1, lifetime_params::DAYS);
        day_hi = (int) params->calendar_matrix.at(n, lifetime_params::DAYS);
        capacity_lo = (int) params->calendar_matrix.at(n - 1, lifetime_params::CAPACITY_CAL);
        capacity_hi = (int) params->calendar_matrix.at(n, lifetime_params::CAPACITY_CAL);
    }

    state->q_relative_calendar = util::interpolate(day_lo, capacity_lo, day_hi, capacity_hi, state->day_age_of_battery);
}

void lifetime_calendar_t::replaceBattery(double replacement_percent) {
    state->day_age_of_battery = 0;
    state->dq_relative_calendar_old = 0;
    state->q_relative_calendar += replacement_percent;
    if (params->calendar_choice == lifetime_params::CALENDAR_CHOICE::MODEL)
        state->q_relative_calendar = fmin(params->calendar_q0 * 100, state->q_relative_calendar);
    if (params->calendar_choice == lifetime_params::CALENDAR_CHOICE::TABLE)
        state->q_relative_calendar = fmin(100, state->q_relative_calendar);
}

/*
Define Lifetime Model
*/

lifetime_state::lifetime_state(){
    q_relative = 0;
    cycle = std::make_shared<cycle_state>();
    calendar = std::make_shared<calendar_state>();
}

lifetime_state::lifetime_state(const std::shared_ptr<cycle_state>& cyc, const std::shared_ptr<calendar_state>& cal) {
    cycle = cyc;
    calendar = cal;
    q_relative = fmin(cycle->q_relative_cycle, calendar->q_relative_calendar);
}

lifetime_state &lifetime_state::operator=(const lifetime_state &rhs) {
    if (this != &rhs) {
        q_relative = rhs.q_relative;
        *cycle = *rhs.cycle;
        *calendar = *rhs.calendar;
    }
    return *this;
}

void lifetime_t::initialize() {
    cycle_model = std::unique_ptr<lifetime_cycle_t>(new lifetime_cycle_t(params));
    calendar_model = std::unique_ptr<lifetime_calendar_t>(new lifetime_calendar_t(params));
    state = std::make_shared<lifetime_state>(cycle_model->state, calendar_model->state);
    state->q_relative = fmin(state->cycle->q_relative_cycle, state->calendar->q_relative_calendar);
}

lifetime_t::lifetime_t(const util::matrix_t<double> &batt_lifetime_matrix, double dt_hour,
                       const util::matrix_t<double> &calendar_matrix) {
    params = std::make_shared<lifetime_params>();
    params->dt_hour = dt_hour;
    params->cycling_matrix = batt_lifetime_matrix;
    params->calendar_choice = lifetime_params::CALENDAR_CHOICE::TABLE;
    params->calendar_matrix = calendar_matrix;

    initialize();
}

lifetime_t::lifetime_t(const util::matrix_t<double> &batt_lifetime_matrix, double dt_hour, double q0, double a, double b,
                       double c) {
    params = std::make_shared<lifetime_params>();
    params->dt_hour = dt_hour;
    params->cycling_matrix = batt_lifetime_matrix;
    params->calendar_choice = lifetime_params::CALENDAR_CHOICE::MODEL;
    params->calendar_q0 = q0;
    params->calendar_a = a;
    params->calendar_b = b;
    params->calendar_c = c;

    initialize();
}

lifetime_t::lifetime_t(const util::matrix_t<double> &batt_lifetime_matrix, double dt_hour) {
    params = std::make_shared<lifetime_params>();
    params->dt_hour = dt_hour;
    params->cycling_matrix = batt_lifetime_matrix;
    params->calendar_choice = lifetime_params::CALENDAR_CHOICE::NONE;

    initialize();
}

lifetime_t::lifetime_t(std::shared_ptr<lifetime_params> params_ptr):
        params(std::move(params_ptr)) {
    initialize();
}

lifetime_t::lifetime_t(const lifetime_t& rhs) {
    state = std::make_shared<lifetime_state>();
    params = std::make_shared<lifetime_params>();
    operator=(rhs);
}

lifetime_t& lifetime_t::operator=(const lifetime_t& rhs) {
    if (this != &rhs) {
        *params = *rhs.params;
        calendar_model = std::unique_ptr<lifetime_calendar_t>(new lifetime_calendar_t(params));
        cycle_model = std::unique_ptr<lifetime_cycle_t>(new lifetime_cycle_t(params));
        state->q_relative = rhs.state->q_relative;
        state->calendar = calendar_model->state;
        state->cycle = cycle_model->state;
    }
    return *this;
}

lifetime_t *lifetime_t::clone() {
    return new lifetime_t(*this);
}

double lifetime_t::capacity_percent() { return state->q_relative; }

double lifetime_t::capacity_percent_cycle() { return cycle_model->capacity_percent(); }

double lifetime_t::capacity_percent_calendar() { return calendar_model->capacity_percent(); }

void lifetime_t::runLifetimeModels(size_t lifetimeIndex, bool charge_changed, double prev_DOD, double DOD, double T_battery) {
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

double lifetime_t::estimateCycleDamage() {
    return cycle_model->estimateCycleDamage();
}

void lifetime_t::replaceBattery(double percent_to_replace) {
    cycle_model->replaceBattery(percent_to_replace);
    calendar_model->replaceBattery(percent_to_replace);
    state->q_relative = fmin(cycle_model->capacity_percent(), calendar_model->capacity_percent());
}

lifetime_params lifetime_t::get_params() { return *params; }

lifetime_state lifetime_t::get_state() {
    lifetime_state state_copy = *state;
    return state_copy;
}
