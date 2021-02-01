#include "lib_battery_lifetime_calendar_cycle.h"
#include "lib_battery_lifetime_nmc.h"
#include "lib_battery_lifetime.h"
#include <numeric>

void lifetime_nmc_t::initialize() {
    
    // cycle model for counting cycles only, no cycle-only degradation
    cycle_model = std::unique_ptr<lifetime_cycle_t>(new lifetime_cycle_t(params, state));
    // do any state initialization here
    state->nmc_state->q_relative_li = 100;
    state->nmc_state->q_relative_neg = 100;
    state->nmc_state->dq_relative_li_old = 0;
    state->nmc_state->dq_relative_neg_old = 0;
    state->nmc_state->day_age_of_battery_float = 0;
    state->nmc_state->DOD_max = 50;
    state->nmc_state->n_cycles_prev_day = 0;
    state->nmc_state->b1_dt.clear();
    state->nmc_state->b2_dt.clear();
    state->nmc_state->b3_dt.clear();
    state->nmc_state->c2_dt.clear();
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

// Rohit - Define negative electrode voltage function
double lifetime_nmc_t::Uneg_computation(double SOC) {
    double Uneg = 0.1;
    if (SOC <= 0.1)
        Uneg = ((0.2420 - 1.2868) / 0.1) * SOC + 1.2868;
    else
        Uneg = ((0.0859 - 0.2420) / 0.9) * (SOC - 0.1) + 0.2420;
    return Uneg;
}

// Rohit - Define open circuit voltage function
double lifetime_nmc_t::Voc_computation(double SOC) {
    double Voc = 0.1;
    if (SOC <= 0.1)
        Voc = ((0.4679) / 0.1) * SOC + 3;
    else if (SOC <= 0.6)
        Voc = ((3.747 - 3.4679) / 0.5) * (SOC - 0.1) + 3.4679;
    else
        Voc = ((4.1934 - 3.7469) / 0.4) * (SOC - 0.6) + 3.7469;
    return Voc;
}

double lifetime_nmc_t::runLifetimeNMC_Qli() {
    double dt_day = 1;
    int dn_cycles = state->n_cycles - state->nmc_state->n_cycles_prev_day;
    double k_cal = 0;
    double b1 = std::accumulate(state->nmc_state->b1_dt.begin(), state->nmc_state->b1_dt.end(), 0);
    double b2 = std::accumulate(state->nmc_state->b2_dt.begin(), state->nmc_state->b1_dt.end(), 0);
    double b3 = std::accumulate(state->nmc_state->b3_dt.begin(), state->nmc_state->b1_dt.end(), 0);

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
    state->nmc_state->q_relative_li = (1 - (dq_new)) * 100;
    return state->nmc_state->q_relative_li;
}

double lifetime_nmc_t::runLifetimeNMC_Qneg(double T_battery, double SOC) {
    double DOD = 1 - SOC; 
    int dn_cycles = state->n_cycles - state->nmc_state->n_cycles_prev_day;

    double c2 = std::accumulate(state->nmc_state->c2_dt.begin(), state->nmc_state->c2_dt.end(), 0);;

    double dq_new;
    if (state->nmc_state->dq_relative_neg_old == 0)
        dq_new = 1 - sqrt(1 - 2 * (c2 / c0_ref) * dn_cycles);
    else
        dq_new = 1 - sqrt(1 - 2 * (c2 / c0_ref) * dn_cycles) + state->nmc_state->dq_relative_neg_old;

    state->nmc_state->dq_relative_neg_old = dq_new;

    state->nmc_state->q_relative_neg = (1 - (dq_new)) * 100;
    return state->nmc_state->q_relative_neg;
}

void lifetime_nmc_t::runLifetimeModels(size_t lifetimeIndex, bool charge_changed, double prev_DOD, double DOD,
                                       double T_battery) {
    double q_last = state->q_relative;
    // update day age of battery
    int day_age_of_battery_old = (int)(state->nmc_state->day_age_of_battery_float);
    state->nmc_state->day_age_of_battery_float = state->nmc_state->day_age_of_battery_float +
        float(util::hours_per_day / params->dt_hr);
    state->day_age_of_battery = (int)(state->nmc_state->day_age_of_battery_float);

    // convert battery temperature to Kelvin
    T_battery += 273; 
    if (charge_changed)
        cycle_model->rainflow(prev_DOD);

    // update DOD_max if DOD > old DOD_max
    if (DOD > state->nmc_state->DOD_max)
        state->nmc_state->DOD_max = DOD;

    //compute open circuit and negative electrode voltage as function of SOC
    double SOC = 0.01 * (100 - DOD);
    double U_neg = Uneg_computation(SOC);
    double V_oc = Voc_computation(SOC);

    // compute lifetime degradation coefficients for current time step,
    //multiply by timestep in days and populate corresponding vectors
    double dt_day = (1 / 24) * params->dt_hr;
    double b1_dt_el = b1_ref * exp(-(Ea_b_1 / Rug) * (1. / T_battery - 1. / T_ref))
        * exp((alpha_a_b1 * F / Rug) * (U_neg / T_battery - U_ref / T_ref))
        * exp(gamma * pow(state->nmc_state->DOD_max, beta_b1)) * dt_day;
    double b2_dt_el = b2_ref * exp(-(Ea_b_2 / Rug) * (1. / T_battery - 1. / T_ref)) * dt_day;
    double b3_dt_el = b3_ref * exp(-(Ea_b_3 / Rug) * (1. / T_battery - 1. / T_ref))
        * exp((alpha_a_b3 * F / Rug) * (V_oc / T_battery - V_ref / T_ref))
        * (1 + theta * state->nmc_state->DOD_max);
    state->nmc_state->b1_dt.push_back(b1_dt_el);
    state->nmc_state->b2_dt.push_back(b2_dt_el);
    state->nmc_state->b3_dt.push_back(b3_dt_el);

    //computations for q_neg
    double c2_dt_el = c2_ref * exp(-(Ea_c_2 / Rug) * (1. / T_battery - 1. / T_ref))
        * pow(DOD, beta_c2);
    state->nmc_state->c2_dt.push_back(c2_dt_el);

    //Run capacity degradation model after every 24 hours
    if (state->day_age_of_battery - day_age_of_battery_old == 1) {
        state->nmc_state->q_relative_li = runLifetimeNMC_Qli();
        state->nmc_state->q_relative_neg = runLifetimeNMC_Qneg(T_battery, SOC);
        state->q_relative = fmin(state->nmc_state->q_relative_li, state->nmc_state->q_relative_neg);
        state->nmc_state->b1_dt.clear();
        state->nmc_state->b2_dt.clear();
        state->nmc_state->b3_dt.clear();
        state->nmc_state->n_cycles_prev_day = state->n_cycles;
    }

    state->q_relative = fmin(state->q_relative, q_last);
}

double lifetime_nmc_t::estimateCycleDamage() {
    return 0;
}

void lifetime_nmc_t::replaceBattery(double percent_to_replace) {
    state->day_age_of_battery = 0;
    state->nmc_state->day_age_of_battery_float = 0;
    state->nmc_state->dq_relative_li_old = 0;
    state->nmc_state->dq_relative_neg_old = 0;
    state->nmc_state->q_relative_li += percent_to_replace;
    state->nmc_state->q_relative_neg += percent_to_replace;
    state->nmc_state->q_relative_li = fmin(100, state->nmc_state->q_relative_li);
    state->nmc_state->q_relative_neg = fmin(100, state->nmc_state->q_relative_neg);
    state->q_relative = fmin(state->nmc_state->q_relative_li, state->nmc_state->q_relative_neg);
}
