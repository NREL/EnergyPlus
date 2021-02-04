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

#ifndef SAM_SIMULATION_CORE_LIB_BATTERY_VOLTAGE_H
#define SAM_SIMULATION_CORE_LIB_BATTERY_VOLTAGE_H

#include <memory>
#include <ostream>
#include <vector>

#include "lib_util.h"
#include "lib_battery_capacity.h"

struct voltage_params {
    enum MODE {
        MODEL, TABLE
    };      // voltage model (0), voltage table (1)

    MODE voltage_choice;
    int num_cells_series;        // number of cells in series
    int num_strings;             // addition number in parallel
    double Vnom_default;         // nominal cell voltage [V]
    double resistance;           // internal cell resistance (Ohm)
    double dt_hr;

    struct {
        double Vfull;
        double Vexp;
        double Vnom;
        double Qfull;
        double Qexp;
        double Qnom;
        double C_rate;
    } dynamic;

    //  depth-of-discharge [%] and cell voltage [V] pairs
    std::vector<std::vector<double>> voltage_table;

    friend std::ostream &operator<<(std::ostream &os, const voltage_params &p);
};

struct voltage_state {
    double cell_voltage;         // closed circuit voltage per cell [V]

    friend std::ostream &operator<<(std::ostream &os, const voltage_state &p);

    bool operator==(const voltage_state &p);
};

/*
Voltage Base class.
All voltage models are based on one-cell, but return the voltage for one battery
*/

class voltage_t {
public:
    voltage_t(int mode, int num_cells_series, int num_strings, double voltage, double dt_hour);

    explicit voltage_t(std::shared_ptr<voltage_params> p);

    voltage_t(const voltage_t &rhs);

    virtual voltage_t &operator=(const voltage_t &rhs);

    virtual voltage_t *clone() = 0;

    virtual ~voltage_t() = default;

    // Call after initialization to set starting V with SOC
    virtual void set_initial_SOC(double init_soc) = 0;

    // Returns estimated max charge power over the next timestep (negative)
    virtual double calculate_max_charge_w(double q, double qmax, double kelvin, double *max_current) = 0;

    // Returns estimated max discharge power over the next timestep
    virtual double calculate_max_discharge_w(double q, double qmax, double kelvin, double *max_current) = 0;

    // Returns current [A] required to dispatch input power [W], only valid if less than max possible
    virtual double calculate_current_for_target_w(double P_watts, double q, double qmax, double kelvin) = 0;

    virtual double calculate_voltage_for_current(double I, double q, double qmax, double T_k) = 0;

    // runs calculate_voltage_for_current and changes the internal cell voltage
    virtual void updateVoltage(double q, double qmax, double I, double temp, double dt) = 0;

    virtual double battery_voltage(); // voltage of one battery

    double battery_voltage_nominal(); // nominal voltage of battery
    double cell_voltage(); // voltage of one cell

    voltage_params get_params();

    voltage_state get_state();

protected:
    std::shared_ptr<voltage_params> params;
    std::shared_ptr<voltage_state> state;

private:
    void initialize();

    friend class battery_t;
};

class voltage_table_t : public voltage_t {
public:
    voltage_table_t(int num_cells_series, int num_strings, double voltage,
                    util::matrix_t<double> &voltage_table, double R, double dt_hour);

    voltage_table_t(std::shared_ptr<voltage_params> p);

    voltage_table_t(const voltage_table_t &rhs);

    voltage_table_t &operator=(const voltage_t &rhs) override;

    voltage_t *clone() override;

    ~voltage_table_t() override = default;

    void set_initial_SOC(double init_soc) override;

    double calculate_max_charge_w(double q, double qmax, double kelvin, double *max_current) override;

    double calculate_max_discharge_w(double q, double qmax, double kelvin, double *max_current) override;

    // return current for targeted power, or 0 if unable
    double calculate_current_for_target_w(double P_watts, double q, double qmax, double kelvin) override;

    double calculate_voltage_for_current(double I, double q, double qmax, double T_k) override;

    void updateVoltage(double q, double qmax, double I, double temp, double dt) override;

protected:
    std::vector<double> slopes;
    std::vector<double> intercepts;

    double calculate_voltage(double DOD);

private:
    void initialize();

};

// Shepard + Tremblay Model
class voltage_dynamic_t : public voltage_t {
public:
    voltage_dynamic_t(int num_cells_series, int num_strings, double voltage, double Vfull,
                      double Vexp, double Vnom, double Qfull, double Qexp, double Qnom,
                      double C_rate, double R, double dt_hr);

    voltage_dynamic_t(std::shared_ptr<voltage_params> p);

    voltage_dynamic_t(const voltage_dynamic_t &rhs);

    voltage_dynamic_t &operator=(const voltage_t &rhs) override;

    voltage_t *clone() override;

    ~voltage_dynamic_t() override = default;

    void set_initial_SOC(double init_soc) override;

    double calculate_max_charge_w(double q, double qmax, double kelvin, double *max_current) override;

    double calculate_max_discharge_w(double q, double qmax, double kelvin, double *max_current) override;

    // returns current for power (discharge > 0, charge < 0), use above functions to first check feasibility
    double calculate_current_for_target_w(double P_watts, double q, double qmax, double kelvin) override;

    double calculate_voltage_for_current(double I, double q, double qmax, double T_k) override;

    
    void updateVoltage(double q, double qmax, double I, double temp, double dt) override; //updates battery voltage based on system capacity (Ah), system maximum capacity (Ah), current (A), temperature (C), and time step (hr)
    

protected:
    double _A;
    double _B0;
    double _E0;
    double _K;

    void parameter_compute();

    double voltage_model_tremblay_hybrid(double Q_cell, double I, double q0_cell);

    // solver quantities
    double solver_Q;
    double solver_q;
    double solver_cutoff_voltage;
    double solver_power;

    void solve_current_for_charge_power(const double *x, double *f);

    void solve_current_for_discharge_power(const double *x, double *f);

private:
    void initialize();

};

// D'Agostino Vanadium Redox Flow Model
class voltage_vanadium_redox_t : public voltage_t {
public:
    voltage_vanadium_redox_t(int num_cells_series, int num_strings, double Vnom_default,
                             double R, double dt_hour);

    explicit voltage_vanadium_redox_t(std::shared_ptr<voltage_params> p);

    voltage_vanadium_redox_t(const voltage_vanadium_redox_t &rhs);

    voltage_vanadium_redox_t &operator=(const voltage_t &rhs) override;

    voltage_t *clone() override;

    ~voltage_vanadium_redox_t() override = default;

    void set_initial_SOC(double init_soc) override;

    double calculate_max_charge_w(double q, double qmax, double kelvin, double *max_current) override;

    double calculate_max_discharge_w(double q, double qmax, double kelvin, double *max_current) override;

    double calculate_current_for_target_w(double P_watts, double q, double qmax, double kelvin) override;

    double calculate_voltage_for_current(double I, double q, double qmax, double T_k) override;

    void updateVoltage(double q, double qmax, double I, double temp, double dt) override;

protected:

    // cell voltage model is on a per-cell basis
    double voltage_model(double q, double qmax, double I_string, double T);

    // RC/F: R is Molar gas constant [J/mol/K]^M, R is Faraday constant [As/mol]^M, C is model correction factor^M 1.38
    double m_RCF;

    // solver quantities
    double solver_Q;
    double solver_q;
    double solver_T_k;              // temp in Kelvin

    double solver_power;

    void solve_current_for_power(const double *x, double *f);

    void solve_max_discharge_power(const double *x, double *f);

private:
    void initialize();

};


#endif //SAM_SIMULATION_CORE_LIB_BATTERY_VOLTAGE_H
