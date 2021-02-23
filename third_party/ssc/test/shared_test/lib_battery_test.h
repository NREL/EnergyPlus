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


#ifndef SYSTEM_ADVISOR_MODEL_LIB_BATTERY_THERMAL_TEST_H
#define SYSTEM_ADVISOR_MODEL_LIB_BATTERY_THERMAL_TEST_H

#include <gtest/gtest.h>

#include "lib_battery.h"
#include "lib_battery_capacity_test.h"
#include "lib_battery_lifetime_test.h"
#include "lib_battery_lifetime_nmc.h"

static void compareState(thermal_state tested_state, thermal_state expected_state, const std::string& msg){
    double tol = 0.02;
    EXPECT_NEAR(tested_state.T_batt, expected_state.T_batt, tol) << msg;
    EXPECT_NEAR(tested_state.T_room, expected_state.T_room, tol) << msg;
    EXPECT_NEAR(tested_state.q_relative_thermal, expected_state.q_relative_thermal, tol) << msg;
    EXPECT_NEAR(tested_state.heat_dissipated, expected_state.heat_dissipated, 1e-3) << msg;
}

static void compareState(const std::shared_ptr<thermal_state>& tested_state, const std::shared_ptr<thermal_state>& expected_state, const std::string& msg){
    compareState(*tested_state, *expected_state, msg);
}

class lib_battery_thermal_test : public ::testing::Test
{
protected:
    std::unique_ptr<thermal_t> model;

    double tol = 0.01;
    double error;

    double mass = 507;
    double surface_area = 0.58 * 0.58 * 6;
    double batt_R = 0.0002;
    double Cp = 1004;
    double h = 20;
    std::vector<double> T_room = {16.85, 16.85, 21.85, 21.85, 16.85, -3.15, -3.15};
    util::matrix_t<double> capacityVsTemperature;

    double dt_hour = 1;
    int nyears = 1;

public:

    void SetUp() override {
        double vals3[] = { -10, 60, 0, 80, 25, 100, 40, 100 };
        capacityVsTemperature.assign(vals3, 4, 2);
    }
    void CreateModel(double Cp){
        model = std::unique_ptr<thermal_t>(new thermal_t(dt_hour, mass, surface_area, batt_R, Cp, h, capacityVsTemperature, T_room));
    }
    void CreateModelSixSecondStep(double Cp) {
        dt_hour = 1.0 / 600.0;
        model = std::unique_ptr<thermal_t>(new thermal_t(dt_hour, mass, surface_area, batt_R, Cp, h, capacityVsTemperature, T_room));
    }
};

class lib_battery_losses_test : public ::testing::Test
{
protected:
    std::unique_ptr<losses_t> model;

    double tol = 0.01;
    double error;

    std::vector<double> chargingLosses;
    std::vector<double> dischargingLosses;

    std::vector<double> fullLosses;

    double dt_hour = 1;
    int nyears = 1;

public:

    void SetUp() override {
        // losses
        for (size_t m = 0; m < 12; m++) {
            chargingLosses.push_back((double)m);
            dischargingLosses.push_back((double)m + 1.);
        }
        for (size_t i = 0; i < 8760; i++) {
            fullLosses.push_back((double)i/8760);
        }
    }
};

struct battery_state_test{
    capacity_state capacity;
    double batt_voltage;
    lifetime_state lifetime;
    thermal_state thermal;

    size_t last_idx;
};

static void compareState(std::unique_ptr<battery_t>&model, const battery_state_test& expected_state, const std::string& msg){
    auto tested_state = model->get_state();
    compareState(*tested_state.capacity, expected_state.capacity, msg);

    EXPECT_NEAR(tested_state.V, expected_state.batt_voltage, 0.01) << msg;

    double tol = 0.01;
    auto lifetime_tested = tested_state.lifetime;
    auto lifetime_expected = expected_state.lifetime;
    EXPECT_NEAR(lifetime_tested->day_age_of_battery, lifetime_expected.day_age_of_battery, tol) << msg;
    EXPECT_NEAR(lifetime_tested->range, lifetime_expected.range, tol) << msg;
    EXPECT_NEAR(lifetime_tested->average_range, lifetime_expected.average_range, tol) << msg;
    EXPECT_NEAR(lifetime_tested->n_cycles, lifetime_expected.n_cycles, tol) << msg;

    auto cal_expected = *lifetime_expected.calendar;
    EXPECT_NEAR(lifetime_tested->calendar->q_relative_calendar, cal_expected.q_relative_calendar, tol) << msg;
    EXPECT_NEAR(lifetime_tested->calendar->dq_relative_calendar_old, cal_expected.dq_relative_calendar_old, tol) << msg;

    auto cyc_expected = *lifetime_expected.cycle;
    EXPECT_NEAR(lifetime_tested->cycle->q_relative_cycle, cyc_expected.q_relative_cycle, tol) << msg;
    EXPECT_NEAR(lifetime_tested->cycle->rainflow_Xlt, cyc_expected.rainflow_Xlt, tol) << msg;
    EXPECT_NEAR(lifetime_tested->cycle->rainflow_Ylt, cyc_expected.rainflow_Ylt, tol) << msg;
    EXPECT_NEAR(lifetime_tested->cycle->rainflow_jlt, cyc_expected.rainflow_jlt, tol) << msg;

    compareState(*tested_state.thermal, expected_state.thermal, msg);

}

class lib_battery_test : public ::testing::Test
{
public:

    // capacity
    double q;
    double SOC_min;
    double SOC_max;
    double SOC_init;

    // voltage
    int n_series;
    int n_strings;
    double Vnom_default;
    double Vfull;
    double Vexp;
    double Vnom;
    double Qfull;
    double Qexp;
    double Qnom;
    double C_rate;
    double resistance;

    // lifetime
    util::matrix_t<double> cycleLifeMatrix;
    util::matrix_t<double> calendarLifeMatrix;
    int calendarChoice;
    int replacementOption;
    double replacementCapacity;

    // thermal
    double mass;
    double surface_area;
    double Cp;
    double h;
    std::vector<double> T_room;
    util::matrix_t<double> capacityVsTemperature;

    // losses
    std::vector<double> monthlyLosses;
    std::vector<double> fullLosses;
    std::vector<double> fullLossesMinute;
    int lossChoice;

    // battery
    int chemistry;
    double dtHour;

    // models
    double tol = 0.02;

    capacity_lithium_ion_t * capacityModel;
    voltage_t * voltageModel;
    thermal_t * thermalModel;
    lifetime_t * lifetimeModel;
    losses_t * lossModel;
    std::unique_ptr<battery_t> batteryModel;

    void SetUp() override
    {
        // capacity
        q = 1000;
        SOC_init = 50;
        SOC_min = 5;
        SOC_max = 95;

        // voltage
        n_series = 139;
        n_strings = 9;
        Vnom_default = 3.6;
        Vfull = 4.1;
        Vexp = 4.05;
        Vnom = 3.4;
        Qfull = 2.25;
        Qexp = 0.04;
        Qnom = 2.0;
        C_rate = 0.2;
        resistance = 0.0002;

        // lifetime
        double vals[] = { 20, 0, 100, 20, 5000, 80, 20, 10000, 60, 80, 0, 100, 80, 1000, 80, 80, 2000, 60 };
        cycleLifeMatrix.assign(vals, 6, 3);
        double vals2[] = { 0, 100, 3650, 80, 7300, 50 };
        calendarLifeMatrix.assign(vals2, 3, 2);
        calendarChoice = 1;
        replacementOption = 0;

        // thermal
        mass = 507;
        surface_area = 0.58 * 0.58 * 6;
        Cp = 1004;
        h = 20;
        T_room.emplace_back(20);

        double vals3[] = { -10, 60, 0, 80, 25, 100, 40, 100 };
        capacityVsTemperature.assign(vals3, 4, 2);

        // losses
        for (size_t m = 0; m < 12; m++) {
            monthlyLosses.push_back((double)m);
        }
        for (size_t i = 0; i < 8760; i++) {
            fullLosses.push_back(0);
        }
        for (size_t i = 0; i < 8760 * 60; i++) {
            fullLossesMinute.push_back(0);
        }
        lossChoice = 0;

        // battery
        chemistry = 1;
        dtHour = 1.0;

        capacityModel = new capacity_lithium_ion_t(q, SOC_init, SOC_max, SOC_min, dtHour);
        voltageModel = new voltage_dynamic_t(n_series, n_strings, Vnom_default, Vfull, Vexp, Vnom, Qfull, Qexp, Qnom,
                                             C_rate, resistance, dtHour);
        lifetimeModel = new lifetime_calendar_cycle_t(cycleLifeMatrix, dtHour, 1.02, 2.66e-3, -7280, 930);
        thermalModel = new thermal_t(dtHour, mass, surface_area, resistance, Cp, h, capacityVsTemperature, T_room);
        lossModel = new losses_t(monthlyLosses, monthlyLosses, monthlyLosses);
        batteryModel = std::unique_ptr<battery_t>(new battery_t(dtHour, chemistry, capacityModel, voltageModel, lifetimeModel, thermalModel, lossModel));
    }

    void TearDown() override {
        // batteryModel takes ownership of component models
    }

};

#endif //SYSTEM_ADVISOR_MODEL_LIB_BATTERY_THERMAL_TEST_H
