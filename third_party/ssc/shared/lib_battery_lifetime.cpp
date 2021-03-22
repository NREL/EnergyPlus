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

#include "lib_battery_lifetime.h"
#include "lib_battery_lifetime_calendar_cycle.h"
#include "lib_battery_lifetime_nmc.h"
#include <cmath>

lifetime_params::lifetime_params() {
    model_choice = CALCYC;
    cal_cyc = std::make_shared<calendar_cycle_params>();
}

lifetime_params &lifetime_params::operator=(const lifetime_params &rhs) {
    if (this != &rhs) {
        dt_hr = rhs.dt_hr;
        model_choice = rhs.model_choice;
        *cal_cyc = *rhs.cal_cyc;
    }
    return *this;
}

lifetime_state::lifetime_state(){
    q_relative = 0;
    n_cycles = 0;
    range = 0;
    average_range = 0;
    day_age_of_battery = 0;
    cycle = std::make_shared<cycle_state>();
    calendar = std::make_shared<calendar_state>();
    nmc_li_neg = std::make_shared<lifetime_nmc_state>();
}

lifetime_state::lifetime_state(const lifetime_state &rhs) :
        lifetime_state() {
    operator=(rhs);
}

lifetime_state::lifetime_state(const std::shared_ptr<cycle_state>& cyc, const std::shared_ptr<calendar_state>& cal) {
    q_relative = 0;
    n_cycles = 0;
    range = 0;
    average_range = 0;
    day_age_of_battery = 0;
    cycle = cyc;
    calendar = cal;
    q_relative = fmin(cycle->q_relative_cycle, calendar->q_relative_calendar);
}

lifetime_state::lifetime_state(const std::shared_ptr<lifetime_nmc_state>& nmc) {
    q_relative = 0;
    n_cycles = 0;
    range = 0;
    average_range = 0;
    day_age_of_battery = 0;
    nmc_li_neg = nmc;
    q_relative = fmin(nmc->q_relative_li, nmc->q_relative_neg);
}


lifetime_state &lifetime_state::operator=(const lifetime_state &rhs) {
    if (this != &rhs) {
        q_relative = rhs.q_relative;
        n_cycles = rhs.n_cycles;
        range = rhs.range;
        average_range = rhs.average_range;
        day_age_of_battery = rhs.day_age_of_battery;
        *cycle = *rhs.cycle;
        *calendar = *rhs.calendar;
        *nmc_li_neg = *rhs.nmc_li_neg;
    }
    return *this;
}

lifetime_t::lifetime_t(const lifetime_t &rhs) {
    state = std::make_shared<lifetime_state>(*rhs.state);
    params = std::make_shared<lifetime_params>(*rhs.params);
}

lifetime_t &lifetime_t::operator=(const lifetime_t &rhs) {
    if (this != &rhs) {
        *params = *rhs.params;
        *state = *rhs.state;
    }
    return *this;
}

double lifetime_t::capacity_percent() { return state->q_relative; }


lifetime_params lifetime_t::get_params() { return *params; }

lifetime_state lifetime_t::get_state() {
    lifetime_state state_copy = *state;
    return state_copy;
}
