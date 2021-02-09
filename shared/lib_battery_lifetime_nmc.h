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

#ifndef SAM_SIMULATION_CORE_LIB_BATTERY_LIFETIME_NMC_H
#define SAM_SIMULATION_CORE_LIB_BATTERY_LIFETIME_NMC_H

#include <memory>

#include "lib_util.h"
#include "lib_battery_lifetime_calendar_cycle.h"

struct lifetime_nmc_state {
    double q_relative_li;                // %, SEI degradation
    double q_relative_neg;               // %, cycle degradation
    double dq_relative_li_old;
    double dq_relative_neg_old;
    double DOD_max;
    int n_cycles_prev_day;

    // for complex cycling of battery, b1 = summation of b1_dt * dt_day over a day
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
    static double calculate_Uneg(double SOC);

    /// Calculate open circuit voltage from SOC
    static double calculate_Voc(double SOC);

protected:

    std::unique_ptr<lifetime_cycle_t> cycle_model;

    /// Capacity degradation due to SEI
    double runQli();

    /// Capacity degradation due to cycles
    double runQneg(double T_battery, double SOC);

    void initialize();

    /// NMC Kandler Smith parameters
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
