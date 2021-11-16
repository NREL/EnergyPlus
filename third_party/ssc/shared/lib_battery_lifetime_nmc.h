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

/**
 * NMC Life Model
 *
 * Based on the model developed by NREL:
 *  K. Smith, A. Saxon, M. Keyser, B. Lundstrom, Ziwei Cao, A. Roc
 *  Life prediction model for grid-connected li-ion battery energy storage system
 *  2017 American Control Conference (ACC) (2017), pp. 4062-4068
 *  https://ieeexplore.ieee.org/document/7963578
 */

// Gas Constant
const double Rug = 8.314;       // J K mol-1
const double T_ref = 298.15;    // K
const double F = 96485;         // A s mol−1


struct lifetime_nmc_state {
    double q_relative_li;                // %, SEI degradation
    double q_relative_neg;               // %, cycle degradation
    double dq_relative_li_old;
    double dq_relative_neg_old;
    double DOD_max;
    int n_cycles_prev_day;

    // for complex cycling of battery, b1 = summation of b1_dt * dt_day over a day
    // lifetime capacity updated after 24 hours elapse.

    double cum_dt;      // cumulated dt elapsed
    double b1_dt;
    double b2_dt;
    double b3_dt;
    double c0_dt;
    double c2_dt;

    friend std::ostream& operator<<(std::ostream& os, const lifetime_nmc_state& p);
};

class lifetime_nmc_t : public lifetime_t {
public:
    lifetime_nmc_t(double dt_hr);

    lifetime_nmc_t(std::shared_ptr<lifetime_params> params_pt);

    lifetime_nmc_t(std::shared_ptr<lifetime_params> params_pt, std::shared_ptr<lifetime_state> state_pt);

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

    /// Reference Anode and Cell potential
    double Uneg_ref = 0.08;     // V
    double V_ref = 3.7;         // V

    /// Capacity degradation due to positive electrode-site-limit
    double d0_ref = 75.1;       // Ah
    double Ea_d0_1 = 4126.0;    // J/mol
    double Ea_d0_2 = 9752000.0; // J/mol
    double Ah_ref = 75.;        // Ah

    /// Capacity degradation due to SEI
    double b0 = 1.07;           // 1
    double b1_ref = 0.003503;   // day^-0.5
    double Ea_b1 = 35392.;      // J mol^-1
    double alpha_a_b1 = -1;     // 1
    double beta_b1 = 2.157;     // 1
    double gamma = 2.472;       // 1

    double b2_ref = 0.00001541; // 1
    double Ea_b_2 = -42800.;    // J mol^-1

    double b3_ref = 0.02805;    // 1
    double Ea_b3 = 42800.;      // J mol^-1
    double alpha_a_b3 = 0.0066; // 1
    double tau_b3 = 5;          // 1
    double theta = 0.135;       // 1

    double runQli(double T_battery_K);

    /// Capacity degradation due to cycles
    double c0_ref = 75.64;      // Ah
    double Ea_c0_ref = 2224.;   // J mol^-1
    double c2_ref = 0.0039193;  // Ah cycle^-1
    double Ea_c2 = -48260.;     // J mol^-1
    double beta_c2 = 4.54;      // 1

    double runQneg();

    /// compute lifetime degradation coefficients for current time step
    void integrateDegParams(double dt_day, double DOD, double T_battery);

    /// Integrate degradation from QLi and Qneg over one day, resets `x_dt` values
    void integrateDegLoss(double DOD, double T_battery);

    void initialize();
};


#endif //SAM_SIMULATION_CORE_LIB_BATTERY_LIFETIME_NMC_H
