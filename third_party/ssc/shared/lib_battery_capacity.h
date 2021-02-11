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

#ifndef SYSTEM_ADVISOR_MODEL_LIB_STORAGE_CAPACITY_H
#define SYSTEM_ADVISOR_MODEL_LIB_STORAGE_CAPACITY_H

#include <memory>

extern double tolerance;
extern double low_tolerance;

struct capacity_state {
    double q0;  // [Ah] - Total capacity at timestep
    double qmax_lifetime; // [Ah] - maximum possible capacity
    double qmax_thermal; // [Ah] - maximum capacity adjusted for temperature affects
    double cell_current;   // [A]  - Current draw during last step
    double I_loss; // [A] - Lifetime and thermal losses
    double SOC; // [%] - State of Charge
    double SOC_prev; // [%] - previous step

    enum {
        CHARGE, NO_CHARGE, DISCHARGE
    };
    int charge_mode; // {CHARGE, NO_CHARGE, DISCHARGE}
    int prev_charge; // {CHARGE, NO_CHARGE, DISCHARGE}
    bool chargeChange; // [true/false] - indicates if charging state has changed since last step

    struct {
        double q1_0; // [Ah] - charge available
        double q2_0; // [Ah] - charge bound
        double q1;  // [Ah]- capacity at discharge rate t1
        double q2;  // [Ah] - capacity at discharge rate t2
    } leadacid;

    friend std::ostream &operator<<(std::ostream &os, const capacity_state &p);

    bool operator==(const capacity_state &p);
};

struct capacity_params {
    double qmax_init; // [Ah] - original maximum capacity
    double initial_SOC; // [%] - Initial SOC
    double maximum_SOC; // [%] - Maximum SOC
    double minimum_SOC; // [%] - Minimum SOC
    double dt_hr; // [hr] - Timestep in hours

    struct {
        // parameters for finding c, k, qmax
        double tn;  // [h] - discharge rate for capacity at qn
        double t2;  // [h] - discharge rate for capacity at q2
        double F1;  // [unitless] - internal ratio computation
        double F2;  // [unitless] - internal ratio computation

        double qn;  //  [Ah] - Capacity at tn hour discharge rate
        double q10; //  [Ah] - Capacity at 10 hour discharge rate
        double q20; // [Ah] - Capacity at 20 hour discharge rate
        double I20; // [A]  - Current at 20 hour discharge rate
    } leadacid;

    friend std::ostream &operator<<(std::ostream &os, const capacity_params &p);
};

/*
Base class from which capacity models derive
Note, all capacity models are based on the capacity of one battery
*/
class capacity_t {
public:

    capacity_t();

    capacity_t(double q, double SOC_init, double SOC_max, double SOC_min, double dt_hour);

    explicit capacity_t(std::shared_ptr<capacity_params> p);

    capacity_t(const capacity_t &rhs);

    virtual capacity_t &operator=(const capacity_t &rhs);

    virtual capacity_t *clone() = 0;

    virtual ~capacity_t() = default;

    // pure virtual functions (abstract) which need to be defined in derived classes
    virtual void updateCapacity(double &I, double dt) = 0;

    virtual void updateCapacityForThermal(double capacity_percent) = 0;

    virtual void updateCapacityForLifetime(double capacity_percent) = 0;

    virtual void replace_battery(double replacement_percent) = 0;

    void change_SOC_limits(double min, double max) {
        params->minimum_SOC = min;
        params->maximum_SOC = max;
    }

    virtual double q1() = 0; // available charge
    virtual double q10() = 0; // capacity at 10 hour discharge rate

    void check_charge_change();

    void check_SOC();

    void update_SOC();

    // common outputs
    double SOC_min();

    double SOC_max();

    double SOC();

    double SOC_prev();

    double q0();

    double qmax();

    double qmax_thermal();

    double I();

    bool chargeChanged();

    double I_loss();

    int charge_operation();

    capacity_params get_params();

    capacity_state get_state();

protected:
    std::shared_ptr<capacity_params> params;
    std::shared_ptr<capacity_state> state;

private:
    void initialize();

    friend class battery_t;

};

/*
KiBaM specific capacity model
*/
class capacity_kibam_t : public capacity_t {
public:

    capacity_kibam_t(double q20, double t1, double q1, double q10, double SOC_init, double SOC_max, double SOC_min,
                     double dt_hr);

    explicit capacity_kibam_t(std::shared_ptr<capacity_params> p);

    capacity_kibam_t(const capacity_kibam_t &rhs);

    capacity_kibam_t &operator=(const capacity_t &rhs) override;

    capacity_t *clone() override;

    ~capacity_kibam_t() override = default;

    void updateCapacity(double &I, double dt) override;

    void updateCapacityForThermal(double capacity_percent) override;

    void updateCapacityForLifetime(double capacity_percent) override;

    void replace_battery(double replacement_percent) override;

    double q1() override; // Available charge
    double q2(); // Bound charge
    double q10() override; // Capacity at 10 hour discharge rate
    double q20(); // Capacity at 20 hour discharge rate

protected:
    // unique to kibam
    double c_compute(double F, double t1, double t2, double k_guess);

    double q1_compute(double q10, double q0, double dt, double I); // may remove some inputs, use class variables
    double q2_compute(double q20, double q0, double dt, double I); // may remove some inputs, use class variables
    double Icmax_compute(double q10, double q0, double dt);

    double Idmax_compute(double q10, double q0, double dt);

    double qmax_compute();

    double qmax_of_i_compute(double T);

    void parameter_compute();

    // model parameters
    double c;  // [0-1] - capacity fraction
    double k;  // [1/hour] - rate constant

private:
    void initialize();

};

/*
Lithium Ion specific capacity model
*/
class capacity_lithium_ion_t : public capacity_t {
public:
    capacity_lithium_ion_t(double q, double SOC_init, double SOC_max, double SOC_min, double dt_hr);

    explicit capacity_lithium_ion_t(std::shared_ptr<capacity_params> p);

    capacity_lithium_ion_t(const capacity_lithium_ion_t &rhs);

    capacity_lithium_ion_t &operator=(const capacity_t &rhs) override;

    capacity_t *clone() override;

    ~capacity_lithium_ion_t() override = default;

    void updateCapacity(double &I, double dt) override;

    void updateCapacityForThermal(double capacity_percent) override;

    void updateCapacityForLifetime(double capacity_percent) override;

    void replace_battery(double replacement_percent) override;

    double q1() override; // Available charge
    double q10() override; // Capacity at 10 hour discharge rate
};


#endif //SYSTEM_ADVISOR_MODEL_LIB_STORAGE_CAPACITY_H
