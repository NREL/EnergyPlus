#ifndef SAM_SIMULATION_CORE_LIB_BATTERY_CAPACITY_TEST_H
#define SAM_SIMULATION_CORE_LIB_BATTERY_CAPACITY_TEST_H

#include <gtest/gtest.h>

//#include "lib_battery_capacity.h"
#include "lib_battery.h"

static void compareState(capacity_state tested_state, capacity_state expected_state, const std::string& msg){
    double tol = 0.01;
    EXPECT_NEAR(tested_state.q0, expected_state.q0, tol) << msg;
    EXPECT_NEAR(tested_state.qmax_thermal, expected_state.qmax_thermal, tol) << msg;
    EXPECT_NEAR(tested_state.qmax_lifetime, expected_state.qmax_lifetime, tol) << msg;
    EXPECT_NEAR(tested_state.cell_current, expected_state.cell_current, tol) << msg;
    EXPECT_NEAR(tested_state.I_loss, expected_state.I_loss, tol) << msg;
    EXPECT_NEAR(tested_state.SOC, expected_state.SOC, tol) << msg;
    EXPECT_NEAR(tested_state.SOC_prev, expected_state.SOC_prev, tol) << msg;
    EXPECT_NEAR(tested_state.charge_mode, expected_state.charge_mode, tol) << msg;
}

static void compareState(const std::shared_ptr<capacity_state>& tested_state, const std::shared_ptr<capacity_state>& expected_state, const std::string& msg){
    compareState(*tested_state, *expected_state, msg);
}

class lib_battery_capacity_test : public ::testing::Test
{
protected:
//    std::unique_ptr<battery_capacity_interface> new_cap;
    std::shared_ptr<capacity_t> old_cap;
//    std::shared_ptr<storage_time_params> time;

//    battery_capacity_params params;
    double tol = 0.1;
    double error;

    double q = 1000;
    double SOC_init = 50;
    double SOC_min = 15;
    double SOC_max = 95;

    double dt_hour = 1;
    int nyears = 1;

};

/**
 * Tests the Lithium Ion capacity model for Li-Ion, Vanadium Redox and Iron flow
 */


class LiIon_lib_battery_capacity_test : public lib_battery_capacity_test
{
public:

    void SetUp()override {
        old_cap = std::make_shared<capacity_lithium_ion_t>(q, SOC_init, SOC_max, SOC_min, dt_hour);
        double I = 1e-7;
        old_cap->updateCapacity(I, 1);
    }
};

/**
 * Tests the Kinetic battery model for Lead acid
 */


class KiBam_lib_battery_capacity_test : public lib_battery_capacity_test
{
protected:
    double q10 = 93.;
    double t1 = 1;
    double q1 = 60.;
    double q20 = 100;
public:

    void SetUp() override {
        old_cap = std::make_shared<capacity_kibam_t>(q20, t1, q1, q10,SOC_init,
                SOC_max, SOC_min, dt_hour);
    }
};
#endif //SAM_SIMULATION_CORE_LIB_BATTERY_CAPACITY_TEST_H
