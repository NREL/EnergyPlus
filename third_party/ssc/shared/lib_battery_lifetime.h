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

#ifndef SAM_SIMULATION_CORE_LIB_BATTERY_LIFETIME_H
#define SAM_SIMULATION_CORE_LIB_BATTERY_LIFETIME_H

#include <memory>

#include "lib_util.h"

/*
Lifetime cycling class.
*/

struct lifetime_params {
    // cycling
    util::matrix_t<double> cycling_matrix;
    enum CYCLING_COLUMNS {
        DOD, CYCLE, CAPACITY_CYCLE
    };

    // calendar
    //Rohit adding MODEL_NMC option
    enum CALENDAR_CHOICE {
        NONE, MODEL, TABLE, NMC_MODEL
    };
    int calendar_choice;
    double dt_hour;

    // K. Smith: Life Prediction model coefficients
    double calendar_q0; // unitless
    double calendar_a;  // 1/sqrt(day)
    double calendar_b;  // K
    double calendar_c;  // K

    // table entries
    util::matrix_t<double> calendar_matrix;
    enum CALENDAR_COLUMNS {
        DAYS, CAPACITY_CAL
    };

    friend std::ostream &operator<<(std::ostream &os, const lifetime_params &p);
};

struct cycle_state {
    double q_relative_cycle;                // %
    //Rohit - Keep track of cycle degradation for Liion NMC
    double dq_relative_cycle_old; 
    int n_cycles;
    double range;
    double average_range;
    enum RAINFLOW_CODES {
        LT_SUCCESS, LT_GET_DATA, LT_RERANGE
    };
    double rainflow_Xlt;
    double rainflow_Ylt;
    int rainflow_jlt;                // last index in Peaks, i.e, if Peaks = [0,1], then jlt = 1
    std::vector<double> rainflow_peaks;

    friend std::ostream &operator<<(std::ostream &os, const cycle_state &p);

    bool operator==(const cycle_state &p);
};

class lifetime_cycle_t {

public:
    /// Constructor for independent model, owning its state and params
    lifetime_cycle_t(const util::matrix_t<double> &batt_lifetime_matrix);

    /// Constructor as lifetime_t component
    explicit lifetime_cycle_t(std::shared_ptr<lifetime_params> params_ptr);

    lifetime_cycle_t(const lifetime_cycle_t &rhs);

    lifetime_cycle_t &operator=(const lifetime_cycle_t &rhs);

    lifetime_cycle_t *clone();

    /// return q, the effective capacity percent
    double runCycleLifetime(double DOD);

    /// Rohit - return q, the effective capacity percent for Lithium Ion NMC
    double runCycleLifetimeNMC(double T, double DOD);

    /// return hypothetical dq the average cycle
    double estimateCycleDamage();

    /// Return the relative capacity percentage of nominal (%)
    double capacity_percent();

    /// Run the rainflow counting algorithm at the current depth-of-discharge to determine cycle
    void rainflow(double DOD);

    /// Replace or partially replace a batteyr
    void replaceBattery(double replacement_percent);

    /// Return the total cycles elapse
    int cycles_elapsed();

    /// Return the range of the last cycle
    double cycle_range();

    /// Return the average cycle range
    double average_range();

    cycle_state get_state();

protected:

    void rainflow_ranges();

    void rainflow_ranges_circular(int index);

    int rainflow_compareRanges();

    /// Bilinear interpolation, given the depth-of-discharge and cycle number, return the capacity percent
    double bilinear(double DOD, int cycle_number);

    std::shared_ptr<cycle_state> state;
    std::shared_ptr<lifetime_params> params;

private:
    void initialize();

    /// Rohit - constants for Q_neg of LiionNMC
    double c0_ref = 75.1;
    double c2_ref = 0.0039193;
    double Ea_c_2 = -48260;
    double Rug = 8.314;
    double T_ref = 298.15;
    double beta_c2 = 4.54; 


    friend class lifetime_t;
};

/*
Lifetime calendar model
*/

struct calendar_state {
    double q_relative_calendar;            // %
    int day_age_of_battery;
    double dq_relative_calendar_old;       // (0 - 1)

    friend std::ostream &operator<<(std::ostream &os, const calendar_state &p);

    bool operator==(const calendar_state &p);
};

class lifetime_calendar_t {
public:
    /// Constructors for independent models, owning its state and params
    lifetime_calendar_t(double dt_hour, const util::matrix_t<double>& calendar_matrix);

    explicit lifetime_calendar_t(double dt_hour, double q0= 1.02, double a= 2.66e-3, double b= -7280, double c= 930);

    /// Constructor as lifetime_t component
    explicit lifetime_calendar_t(std::shared_ptr<lifetime_params> params_ptr);

    lifetime_calendar_t(const lifetime_calendar_t &rhs);

    lifetime_calendar_t &operator=(const lifetime_calendar_t &rhs);

    lifetime_calendar_t *clone();

    /// Given the index of the simulation, the tempertature and SOC, return the effective capacity percent
    /// Rohit - Add parameter charge_changed
    double runLifetimeCalendarModel(size_t lifetimeIndex, double T, double SOC, bool charge_changed);

    /// Reset or augment the capacity
    void replaceBattery(double replacement_percent);

    /// Return the relative capacity percentage of nominal (%)
    double capacity_percent();

    /// Calculate negative electrode voltage from SOC
    double Uneg_computation(double SOC);

    /// Calculate open circuit voltage from SOC
    double Voc_computation(double SOC);

    calendar_state get_state();

protected:
    void runLithiumIonModel(double temp_C, double SOC);

    // Rohit - add Q_li for Li_ion model
    void runLithiumIonNMCModel(double temp_C, double SOC, bool charge_changed);

    void runTableModel();

    double dt_day;

    std::shared_ptr<calendar_state> state;
    std::shared_ptr<lifetime_params> params;

    //Rohit - Add cycle_model object in lifetime_calendar_t
    std::unique_ptr<lifetime_cycle_t> cycle_model;

private:
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

    friend class lifetime_t;
};

/*
Class to encapsulate multiple lifetime models, and linearly combined the associated degradation and handle replacements
*/

struct lifetime_state {
    double q_relative;                      // total lifetime relative capacity %

    std::shared_ptr<cycle_state> cycle;
    std::shared_ptr<calendar_state> calendar;

    lifetime_state();

    lifetime_state(const std::shared_ptr<cycle_state>& cyc, const std::shared_ptr<calendar_state>& cal);

    lifetime_state &operator=(const lifetime_state &rhs);

    friend std::ostream &operator<<(std::ostream &os, const lifetime_state &p);
};

class lifetime_t {
public:
    /// Cycle with Calendar table
    lifetime_t(const util::matrix_t<double> &batt_lifetime_matrix,
               double dt_hour, const util::matrix_t<double>& calendar_matrix);

    /// Cycle with Calendar model
    lifetime_t(const util::matrix_t<double> &batt_lifetime_matrix,
               double dt_hour, double q0, double a, double b, double c);

    /// Rohit NMC model constructor 
    lifetime_t(double dt_hour);

    /// Cycle with no Calendar
    lifetime_t(const util::matrix_t<double> &batt_lifetime_matrix, double dt_hour);

    explicit lifetime_t(std::shared_ptr<lifetime_params> params_ptr);

    lifetime_t(const lifetime_t& rhs);

    lifetime_t &operator=(const lifetime_t& rhs);

    virtual ~lifetime_t() {};

    lifetime_t *clone();

    /// Execute the lifetime models given the current lifetime run index, capacity model, and temperature
    void runLifetimeModels(size_t lifetimeIndex, bool charge_changed, double prev_DOD, double DOD, double T_battery);

    double estimateCycleDamage();

    void replaceBattery(double percent_to_replace);

    /// Return the relative capacity percentage of nominal (%)
    double capacity_percent();

    /// Return the relative capacity percentage of nominal caused by cycle damage (%)
    double capacity_percent_cycle();

    /// Return the relative capacity percentage of nominal caused by calendar fade (%)
    double capacity_percent_calendar();

    lifetime_params get_params();

    lifetime_state get_state();

protected:

    std::shared_ptr<lifetime_state> state;
    std::shared_ptr<lifetime_params> params;

    std::unique_ptr<lifetime_calendar_t> calendar_model;
    std::unique_ptr<lifetime_cycle_t> cycle_model;

private:
    void initialize();

    friend class battery_t;
};


#endif //SAM_SIMULATION_CORE_LIB_BATTERY_LIFETIME_H
