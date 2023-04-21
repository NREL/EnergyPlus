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

#ifndef SAM_SIMULATION_CORE_LIB_BATTERY_LIFETIME_CALENDAR_CYCLE_H
#define SAM_SIMULATION_CORE_LIB_BATTERY_LIFETIME_CALENDAR_CYCLE_H

#include <memory>

#include "lib_util.h"
#include "lib_battery_lifetime.h"


/*
Lifetime cycling class.
*/

struct calendar_cycle_params {
    // cycling
    util::matrix_t<double> cycling_matrix;
    enum CYCLING_COLUMNS {
        DOD, CYCLE, CAPACITY_CYCLE
    };

    // calendar
    enum CALENDAR_CHOICE {
        NONE, MODEL, TABLE
    };
    int calendar_choice;

    double calendar_q0; // unitless
    double calendar_a;  // 1/sqrt(day)
    double calendar_b;  // K
    double calendar_c;  // K

    // table entries
    util::matrix_t<double> calendar_matrix;
    enum CALENDAR_COLUMNS {
        DAYS, CAPACITY_CAL
    };

    friend std::ostream &operator<<(std::ostream &os, const calendar_cycle_params &p);
};

struct cycle_state {
    double q_relative_cycle;                // %
    enum RAINFLOW_CODES {
        LT_SUCCESS, LT_GET_DATA, LT_RERANGE
    };
    double rainflow_Xlt;
    double rainflow_Ylt;
    int rainflow_jlt;                // last index in Peaks, i.e, if Peaks = [0,1], then jlt = 1
    std::vector<double> rainflow_peaks;

    friend std::ostream &operator<<(std::ostream &os, const cycle_state &p);
};

class lifetime_cycle_t {

public:
    /// Constructor for independent model, owning its state and params
    lifetime_cycle_t(const util::matrix_t<double> &batt_lifetime_matrix);

    /// Constructor as lifetime_calendar_cycle_t component
    explicit lifetime_cycle_t(std::shared_ptr<lifetime_params> params_ptr);

    lifetime_cycle_t(std::shared_ptr<lifetime_params> params_ptr, std::shared_ptr<lifetime_state> state_ptr);

    lifetime_cycle_t(const lifetime_cycle_t &rhs);

    lifetime_cycle_t &operator=(const lifetime_cycle_t &rhs);

    lifetime_cycle_t *clone();

    /// return q, the effective capacity percent
    double runCycleLifetime(double DOD);

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

    lifetime_state get_state();

protected:

    void rainflow_ranges();

    void rainflow_ranges_circular(int index);

    int rainflow_compareRanges();

    /// Bilinear interpolation, given the depth-of-discharge and cycle number, return the capacity percent
    double bilinear(double DOD, int cycle_number);

    std::shared_ptr<lifetime_params> params;

    std::shared_ptr<lifetime_state> state;

private:
    void initialize();

    friend class lifetime_calendar_cycle_t;
};

/*
Lifetime calendar model
*/

struct calendar_state {
    double q_relative_calendar;            // %
    double dq_relative_calendar_old;       // (0 - 1)
//    int day_age_of_battery;

    friend std::ostream &operator<<(std::ostream &os, const calendar_state &p);

    bool operator==(const calendar_state &p) const;
};

class lifetime_calendar_t {
public:
    /// Constructors for independent models, owning its state and params
    lifetime_calendar_t(double dt_hour, const util::matrix_t<double>& calendar_matrix);

    explicit lifetime_calendar_t(double dt_hour, double q0= 1.02, double a= 2.66e-3, double b= -7280, double c= 930);

    lifetime_calendar_t(std::shared_ptr<lifetime_params> params_ptr, std::shared_ptr<lifetime_state> state_ptr);

    lifetime_calendar_t(const lifetime_calendar_t &rhs);

    lifetime_calendar_t &operator=(const lifetime_calendar_t &rhs);

    lifetime_calendar_t *clone();

    /// Given the index of the simulation, the tempertature and SOC, return the effective capacity percent
    double runLifetimeCalendarModel(size_t lifetimeIndex, double T, double SOC);

    /// Reset or augment the capacity
    void replaceBattery(double replacement_percent);

    /// Return the relative capacity percentage of nominal (%)
    double capacity_percent();

    lifetime_state get_state();

protected:
    void runLithiumIonModel(double temp_C, double SOC);

    void runTableModel();

    double dt_day;

    std::shared_ptr<lifetime_params> params;

    std::shared_ptr<lifetime_state> state;

private:
    void initialize();

    friend class lifetime_calendar_cycle_t;
};

/*
Class to encapsulate multiple lifetime models, and linearly combined the associated degradation and handle replacements
*/

class lifetime_calendar_cycle_t : public lifetime_t {
public:
    /// Cycle with Calendar table
    lifetime_calendar_cycle_t(const util::matrix_t<double> &batt_lifetime_matrix,
                              double dt_hour, const util::matrix_t<double>& calendar_matrix);

    /// Cycle with Calendar model
    lifetime_calendar_cycle_t(const util::matrix_t<double> &batt_lifetime_matrix,
                              double dt_hour, double q0, double a, double b, double c);

    /// Cycle with no Calendar
    lifetime_calendar_cycle_t(const util::matrix_t<double> &batt_lifetime_matrix, double dt_hour);

    lifetime_calendar_cycle_t(std::shared_ptr<lifetime_params> params_ptr);

    lifetime_calendar_cycle_t(const lifetime_calendar_cycle_t& rhs);

    lifetime_calendar_cycle_t &operator=(const lifetime_calendar_cycle_t& rhs);

    lifetime_calendar_cycle_t *clone() override;

    ~lifetime_calendar_cycle_t() override = default;

    /// Execute the lifetime models given the current lifetime run index, capacity model, and temperature
    void runLifetimeModels(size_t lifetimeIndex, bool charge_changed, double prev_DOD, double DOD, double T_battery) override;

    double estimateCycleDamage() override;

    void replaceBattery(double percent_to_replace) override;

    /// Return the relative capacity percentage of nominal caused by cycle damage (%)
    double capacity_percent_cycle();

    /// Return the relative capacity percentage of nominal caused by calendar fade (%)
    double capacity_percent_calendar();

protected:

    std::unique_ptr<lifetime_calendar_t> calendar_model;
    std::unique_ptr<lifetime_cycle_t> cycle_model;

private:
    void initialize();

    friend class battery_t;
};


#endif //SAM_SIMULATION_CORE_LIB_BATTERY_LIFETIME_CALENDAR_CYCLE_H
