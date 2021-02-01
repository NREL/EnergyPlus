#include "lib_battery_lifetime.h"
#include "lib_battery_lifetime_calendar_cycle.h"
#include "lib_battery_lifetime_nmc.h"

lifetime_params::lifetime_params() {
    model_choice = CALCYC;
    cal_cyc = std::make_shared<calendar_cycle_params>();
}

lifetime_params &lifetime_params::operator=(const lifetime_params &rhs) {
    if (this != &rhs) {
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
    nmc_state = std::make_shared<lifetime_nmc_state>();
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

//Rohit - define constructor for lifetime_state with lifetime_nmc_state as parameter
lifetime_state::lifetime_state(const std::shared_ptr<lifetime_nmc_state>& nmc) {
    q_relative = 0;
    n_cycles = 0;
    range = 0;
    average_range = 0;
    day_age_of_battery = 0;
    nmc_state = nmc;
    q_relative = fmin(nmc_state->q_relative_li, nmc_state->q_relative_neg);
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
        *nmc_state = *rhs.nmc_state;
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
