#ifndef SAM_SIMULATION_CORE_LIB_BATTERY_LIFETIME_NMC_H
#define SAM_SIMULATION_CORE_LIB_BATTERY_LIFETIME_NMC_H

#include <memory>

#include "lib_util.h"
#include "lib_battery_lifetime_calendar_cycle.h"

// Rohit - add struct for lithiumIonnmc states, SEI degradation (q_relative_li) and cycle degradation (q_relative_neg)

struct lifetime_nmc_state {
    double q_relative_li;                // %
    double q_relative_neg;
    double dq_relative_li_old;
    double dq_relative_neg_old;
    double DOD_max;
    int n_cycles_prev_day;

    float day_age_of_battery_float; // keep track of age of battery with changing timestep

    // for complex cycling of battery, b1 = summagion of b1_dt * dt_day over a day
    // lifetime capacity updated after 24 hours elapse.

    double b1_dt; 
    double b2_dt;
    double b3_dt;
    double c2_dt; 

    friend std::ostream& operator<<(std::ostream& os, const lifetime_nmc_state& p);
};

class lifetime_nmc_t : public lifetime_t {
public:
    lifetime_nmc_t(double dt_hr);

    lifetime_nmc_t(std::shared_ptr<lifetime_params> params_pt);

    lifetime_nmc_t(const lifetime_nmc_t& rhs);

    lifetime_nmc_t &operator=(const lifetime_nmc_t& rhs);

    lifetime_t *clone() override;

    /// Execute the lifetime models given the current lifetime run index, capacity model, and temperature
    void runLifetimeModels(size_t lifetimeIndex, bool charge_changed, double prev_DOD, double DOD, double T_battery) override;

    double estimateCycleDamage() override;

    void replaceBattery(double percent_to_replace) override;

    /// Calculate negative electrode voltage from SOC
    double Uneg_computation(double SOC);

    /// Calculate open circuit voltage from SOC
    double Voc_computation(double SOC);

    // Capacity degradation due to SEI 
    double runLifetimeNMC_Qli();

    // Capacity degradation due to cycles
    double runLifetimeNMC_Qneg(double T_battery, double SOC);

protected:
    std::unique_ptr<lifetime_cycle_t> cycle_model;

    void initialize();

    /// Rohit - Add Li-ion NMC Kandler Smith parameters
    double Ea_d0_1 = 4126.0;
    double b1_ref = 0.003503;
    double Ea_b_1 = 35392.;
    double Rug = 8.314;
    double T_ref = 298.15;
    double alpha_a_b1 = -1;
    double F = 96485;
    double U_ref = 0.08;
    double gamma = 2.472;
    double beta_b1 = 2.157;

    double b2_ref = 0.00001541;
    double Ea_b_2 = -42800.;

    double b3_ref = 0.003503;
    double Ea_b_3 = 42800.;
    double alpha_a_b3 = 0.0066;
    double V_ref = 3.7;
    double theta = 0.135;
    double tau_b3 = 5;

    double c0_ref = 75.1;
    double c2_ref = 0.0039193;
    double Ea_c_2 = -48260;
    double beta_c2 = 4.54;
};


#endif //SAM_SIMULATION_CORE_LIB_BATTERY_LIFETIME_NMC_H
