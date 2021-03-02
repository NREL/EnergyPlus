#include <gtest/gtest.h>
#include <random>

//#include "lib_battery_capacity.h"
#include "lib_battery.h"
#include "lib_battery_lifetime_test.h"

TEST_F(lib_battery_lifetime_cycle_test, SetUpTest) {
    EXPECT_EQ(cycle_model->capacity_percent(), 100);
}


struct cycle_lifetime_state {
    double relative_q;
    double Xlt;
    double Ylt;
    double Range;
    double average_range;
    size_t nCycles;
    double jlt;
    std::vector<double> Peaks;
};

TEST_F(lib_battery_lifetime_cycle_test, runCycleLifetimeTest) {
    double DOD = 5;       // not used but required for function
    int idx = 0;
    while (idx < 500){
        if (idx % 2 != 0){
            DOD = 95;
        }
        else
            DOD = 5;
        cycle_model->runCycleLifetime(DOD);
        idx++;
    }
    lifetime_state s = cycle_model->get_state();
    EXPECT_NEAR(s.cycle->q_relative_cycle, 95.02, tol);
    EXPECT_NEAR(s.cycle->rainflow_Xlt, 90, tol);
    EXPECT_NEAR(s.cycle->rainflow_Ylt, 90, tol);
    EXPECT_NEAR(s.cycle->rainflow_jlt, 2, tol);
    EXPECT_NEAR(s.range, 90, tol);
    EXPECT_NEAR(s.average_range, 90, tol);
    EXPECT_NEAR(s.n_cycles, 249, tol);

    while (idx < 1000){
        if (idx % 2 != 0){
            DOD = 90;
        }
        cycle_model->runCycleLifetime(DOD);
        idx++;
    }
    s = cycle_model->get_state();
    EXPECT_NEAR(s.cycle->q_relative_cycle, 91.244, tol);
    EXPECT_NEAR(s.cycle->rainflow_Xlt, 0, tol);
    EXPECT_NEAR(s.cycle->rainflow_Ylt, 0, tol);
    EXPECT_NEAR(s.cycle->rainflow_jlt, 2, tol);
    EXPECT_NEAR(s.range, 0, tol);
    EXPECT_NEAR(s.average_range, 44.9098, tol);
    EXPECT_NEAR(s.n_cycles, 499, tol);
}

TEST_F(lib_battery_lifetime_cycle_test, runCycleLifetimeTestJaggedProfile) {
    std::vector<double> DOD = { 5, 95, 50, 85, 10, 50, 5, 95, 5 };  // 4 cycles
    int idx = 0;
    while (idx < DOD.size()) {
        cycle_model->runCycleLifetime(DOD[idx]);
        idx++;
    }
    lifetime_state s = cycle_model->get_state();
    EXPECT_NEAR(s.cycle->q_relative_cycle, 99.95, tol);
    EXPECT_NEAR(s.cycle->rainflow_Xlt, 90, tol);
    EXPECT_NEAR(s.cycle->rainflow_Ylt, 90, tol);
    EXPECT_NEAR(s.cycle->rainflow_jlt, 1, tol);
    EXPECT_NEAR(s.range, 90, tol);
    EXPECT_NEAR(s.average_range, 63.75, tol);
    EXPECT_NEAR(s.n_cycles, 4, tol);

}

TEST_F(lib_battery_lifetime_cycle_test, runCycleLifetimeTestKokamProfile) {
    std::vector<double> DOD = { 0.66, 1.0, 0.24722075172048893, 1.0, 0.24559790735021855, 0.9989411900454035, 0.24559790735021936, 0.9989411900454057, 0.24573025859454606, 0.9990735412897335, 0.24625966357184892, 0.9992058925340614, 0.2466567173048243, 0.9992058925340647, 0.2465243660605033, 0.9982794338237967, 0.24718612228213058, 0.9992058925340731, 0.24718612228213466, 0.9982794338238032, 0.24612731232753976, 0.9981470825794796, 0.24625966357186685, 0.9984117850681331, 0.24678906854916766, 0.9984117850681358, 0.24731847352646982, 0.9985441363124643, 0.24784787850377074, 0.9988088388011173, 0.24784787850377454 };  // 4 cycles
    int idx = 0;
    while (idx < DOD.size()) {
        cycle_model->runCycleLifetime((1-DOD[idx]) * 100.0);
        idx++;
    }
    lifetime_state s = cycle_model->get_state();
    EXPECT_NEAR(s.cycle->q_relative_cycle, 99.79, tol);
    EXPECT_NEAR(s.cycle->rainflow_Xlt, 75.09, tol);
    EXPECT_NEAR(s.cycle->rainflow_Ylt, 75.27, tol);
    EXPECT_NEAR(s.cycle->rainflow_jlt, 5, tol);
    EXPECT_NEAR(s.range, 75.07, tol);
    EXPECT_NEAR(s.average_range, 72.03, tol);
    EXPECT_NEAR(s.n_cycles, 13, tol);

}

TEST_F(lib_battery_lifetime_cycle_test, runCycleLifetimeTestWithNoise) {
    int seed = 100;
    double tol_high = 1.6; // Randomness will generate different results on different platforms

    // Initialize a default_random_engine with the seed
    std::default_random_engine randomEngine(seed);

    // Initialize a uniform_real_distribution to produce values between -1 and 1
    std::uniform_real_distribution<double> unifRealDist(-1.0, 1.0);

    double DOD = 5;       // not used but required for function
    int idx = 0;
    while (idx < 500) {
        double number = unifRealDist(randomEngine);
        if (idx % 2 != 0) {
            DOD = 95 + number;
        }
        else
            DOD = 5 + number;
        cycle_model->runCycleLifetime(DOD);
        idx++;
    }
    lifetime_state s = cycle_model->get_state();
    EXPECT_NEAR(s.cycle->q_relative_cycle, 95.06, tol_high);
    EXPECT_NEAR(s.range, 90.6, tol_high);
    EXPECT_NEAR(s.average_range, 90.02, tol_high);

}

TEST_F(lib_battery_lifetime_cycle_test, replaceBatteryTest) {
    double DOD = 5;       // not used but required for function
    int idx = 0;
    while (idx < 1500){
        if (idx % 2 != 0){
            DOD = 95;
        }
        else
            DOD = 5;
        cycle_model->runCycleLifetime(DOD);
        idx++;
    }
    auto st = cycle_lifetime_state({85.02,90,90,90,90, 749, 2});
    lifetime_state s = cycle_model->get_state();
    EXPECT_NEAR(s.cycle->q_relative_cycle, 85.02, tol);
    EXPECT_NEAR(s.cycle->rainflow_Xlt, 90, tol);
    EXPECT_NEAR(s.cycle->rainflow_Ylt, 90, tol);
    EXPECT_NEAR(s.cycle->rainflow_jlt, 2, tol);
    EXPECT_NEAR(s.range, 90, tol);
    EXPECT_NEAR(s.average_range, 90, tol);
    EXPECT_NEAR(s.n_cycles, 749, tol);

    cycle_model->replaceBattery(5);

    s = cycle_model->get_state();
    EXPECT_NEAR(s.cycle->q_relative_cycle, 90.019, tol);
    EXPECT_NEAR(s.cycle->rainflow_Xlt, 0, tol);
    EXPECT_NEAR(s.cycle->rainflow_Ylt, 0, tol);
    EXPECT_NEAR(s.cycle->rainflow_jlt, 0, tol);
    EXPECT_NEAR(s.range, 0, tol);
    EXPECT_NEAR(s.average_range, 90, tol);
    EXPECT_NEAR(s.n_cycles, 749, tol);
}

TEST_F(lib_battery_lifetime_calendar_matrix_test, runCalendarMatrixTest) {
    double T = 278, SOC = 20;       // not used but required for function
    int idx = 0;
    while (idx < 500){
        if (idx % 2 != 0){
            SOC = 90;
        }
        cal_model->runLifetimeCalendarModel(idx, T, SOC);
        idx++;
    }
    lifetime_state s = cal_model->get_state();
    EXPECT_NEAR(s.day_age_of_battery, 20.79, tol);
    EXPECT_NEAR(s.calendar->q_relative_calendar, 99.89, tol);
    EXPECT_NEAR(s.calendar->dq_relative_calendar_old, 0, tol);

    while (idx < 1000){
        if (idx % 2 != 0){
            SOC = 90;
        }
        cal_model->runLifetimeCalendarModel(idx, T, SOC);
        idx++;
    }
    s = cal_model->get_state();
    EXPECT_NEAR(s.day_age_of_battery, 41.625, tol);
    EXPECT_NEAR(s.calendar->q_relative_calendar, 99.775, tol);
    EXPECT_NEAR(s.calendar->dq_relative_calendar_old, 0, tol);
}

TEST_F(lib_battery_lifetime_calendar_matrix_test, replaceBatteryTest) {
    double T = 4.85, SOC = 20;
    int idx = 0;
    while (idx < 200000){
        if (idx % 2 != 0){
            SOC = 90;
        }
        cal_model->runLifetimeCalendarModel(idx, T, SOC);
        idx++;
    }
    lifetime_state s = cal_model->get_state();
    EXPECT_NEAR(s.day_age_of_battery, 8333.29, tol);
    EXPECT_NEAR(s.calendar->q_relative_calendar, 41.51, tol);
    EXPECT_NEAR(s.calendar->dq_relative_calendar_old, 0, tol);

    cal_model->replaceBattery(5);

    s = cal_model->get_state();
    EXPECT_NEAR(s.day_age_of_battery, 0, tol);
    EXPECT_NEAR(s.calendar->q_relative_calendar, 46.51, tol);
    EXPECT_NEAR(s.calendar->dq_relative_calendar_old, 0, tol);
}

TEST_F(lib_battery_lifetime_calendar_model_test, SetUpTest) {
    EXPECT_EQ(cal_model->capacity_percent(), 102);
}

TEST_F(lib_battery_lifetime_calendar_model_test, runCalendarModelTest) {
    double T = 4.85, SOC = 20;       // not used but required for function
    int idx = 0;
    while (idx < 500){
        if (idx % 2 != 0){
            SOC = 90;
        }
        cal_model->runLifetimeCalendarModel(idx, T, SOC);
        idx++;
    }
    lifetime_state s = cal_model->get_state();
    EXPECT_NEAR(s.day_age_of_battery, 20.79, tol);
    EXPECT_NEAR(s.calendar->q_relative_calendar, 101.78, tol);
    EXPECT_NEAR(s.calendar->dq_relative_calendar_old, 0.00217, tol);

    while (idx < 1000){
        if (idx % 2 != 0){
            SOC = 90;
        }
        cal_model->runLifetimeCalendarModel(idx, T, SOC);
        idx++;
    }
    s = cal_model->get_state();
    EXPECT_NEAR(s.day_age_of_battery, 41.625, tol);
    EXPECT_NEAR(s.calendar->q_relative_calendar, 101.69, tol);
    EXPECT_NEAR(s.calendar->dq_relative_calendar_old, 0.00306, tol);
}

TEST_F(lib_battery_lifetime_calendar_model_test, replaceBatteryTest) {
    double T = 4.85, SOC = 20;
    int idx = 0;
    while (idx < 200000){
        if (idx % 2 != 0){
            SOC = 90;
        }
        cal_model->runLifetimeCalendarModel(idx, T, SOC);
        idx++;
    }
    lifetime_state s = cal_model->get_state();
    EXPECT_NEAR(s.day_age_of_battery, 8333.29, tol);
    EXPECT_NEAR(s.calendar->q_relative_calendar, 97.67, tol);
    EXPECT_NEAR(s.calendar->dq_relative_calendar_old, 0.043, tol);

    cal_model->replaceBattery(5);

    s = cal_model->get_state();
    EXPECT_NEAR(s.day_age_of_battery, 0, tol);
    EXPECT_NEAR(s.calendar->q_relative_calendar, 102, tol);
    EXPECT_NEAR(s.calendar->dq_relative_calendar_old, 0.0, tol);
}

TEST_F(lib_battery_lifetime_calendar_matrix_test, TestLifetimeDegradation) {
    double vals[] = { 0, 100, 365, 50 };
    util::matrix_t<double> lifetime_matrix;
    lifetime_matrix.assign(vals, 2, 2);

    double dt_hour = 1;
    lifetime_calendar_t hourly_lifetime(dt_hour, lifetime_matrix);

    for (int idx = 0; idx < 8760; idx++) {
        hourly_lifetime.runLifetimeCalendarModel(idx, 20, 80);
    }

    EXPECT_NEAR(hourly_lifetime.capacity_percent(), 50, 1);

    dt_hour = 1.0 / 12.0; // Every 5 mins
    lifetime_calendar_t subhourly_lifetime(dt_hour, lifetime_matrix);

    for (int idx = 0; idx < 8760 * 12; idx++) {
        subhourly_lifetime.runLifetimeCalendarModel(idx, 20, 80);
    }

    EXPECT_NEAR(subhourly_lifetime.capacity_percent(), 50, 1);
}


TEST_F(lib_battery_lifetime_calendar_model_test, TestLifetimeDegradation) {

    for (int idx = 0; idx < 8760; idx++) {
        cal_model->runLifetimeCalendarModel(idx, 20, 80);
    }

    EXPECT_NEAR(cal_model->capacity_percent(), 99.812, 1);

    dt_hour = 1.0 / 12.0; // Every 5 mins
    lifetime_calendar_t subhourly_lifetime(dt_hour);

    for (int idx = 0; idx < 8760 * 12; idx++) {
        subhourly_lifetime.runLifetimeCalendarModel(idx, 20, 80);
    }

    EXPECT_NEAR(subhourly_lifetime.capacity_percent(), 99.812, 1);
}

TEST_F(lib_battery_lifetime_test, updateCapacityTest) {
    size_t idx = 0;
    while (idx < 876){
        model->runLifetimeModels(idx, true, 5,95, 25);
        model->runLifetimeModels(idx, true, 95, 5, 25);

        auto state = model->get_state();
        EXPECT_EQ(state.cycle->q_relative_cycle, model->capacity_percent_cycle());
        EXPECT_EQ(state.calendar->q_relative_calendar, model->capacity_percent_calendar());

        idx ++;
    }
}

TEST_F(lib_battery_lifetime_test, runCycleLifetimeTestWithRestPeriod) {
    double tol = 0.01;

    std::vector<double> DOD = { 5, 50, 95, 50, 5, 5, 5, 50, 95, 50, 5, 5, 5, 50, 95, 50, 5 };  // 3 cycles 90% DOD
    std::vector<bool> charge_changed = { true, false, false, true, false, false, false, true, false, true, false, false, false, true, false, true, false };
    int idx = 0;
    double T_battery = 25; // deg C
    while (idx < DOD.size()) {
        double DOD_prev = 0;
        if (idx > 0) {
            DOD_prev = DOD[idx - 1];
        }
        model->runLifetimeModels(idx, charge_changed[idx], DOD_prev, DOD[idx], T_battery);
        idx++;
    }


    lifetime_state s = model->get_state();
    EXPECT_NEAR(s.cycle->q_relative_cycle, 99.96, tol);
    EXPECT_NEAR(s.cycle->rainflow_Xlt, 90, tol);
    EXPECT_NEAR(s.cycle->rainflow_Ylt, 90, tol);
    EXPECT_NEAR(s.cycle->rainflow_jlt, 2, tol);
    EXPECT_NEAR(s.range, 90, tol);
    EXPECT_NEAR(s.average_range, 90, tol);
    EXPECT_NEAR(s.n_cycles, 2, tol);
}

TEST_F(lib_battery_lifetime_nmc_test, updateCapacityTest) {
    size_t idx = 0;
    double tol = 0.01;

    //check lifetime_nmc_state_initialization
    ASSERT_EQ(model->get_state().nmc_state->q_relative_neg, 100);
    ASSERT_EQ(model->get_state().nmc_state->q_relative_li, 100);
    ASSERT_EQ(model->get_state().nmc_state->b1_dt, 0);
    ASSERT_EQ(model->get_state().nmc_state->b2_dt, 0);
    ASSERT_EQ(model->get_state().nmc_state->b3_dt, 0);
    ASSERT_EQ(model->get_state().day_age_of_battery, 0);

    //check U_neg, and Voc functions (SOC as a fractional input)
    ASSERT_NEAR(model->calculate_Uneg(0.1), 0.242, tol);
    ASSERT_NEAR(model->calculate_Voc(0.1), 3.4679, tol);
    ASSERT_NEAR(model->calculate_Uneg(0.5), 0.1726, tol);
    ASSERT_NEAR(model->calculate_Voc(0.5), 3.6912, tol);
    ASSERT_NEAR(model->calculate_Uneg(0.9), 0.1032, tol);
    ASSERT_NEAR(model->calculate_Voc(0.9), 4.0818, tol);

    // check number of cycles
    while (idx < 876){
        model->runLifetimeModels(idx, true, 5,95, 25);
        model->runLifetimeModels(idx, true, 95, 5, 25);

        auto state = model->get_state();

        idx ++;
    }

    ASSERT_EQ(model->get_state().n_cycles, 875);

}

TEST_F(lib_battery_lifetime_nmc_test, NoCyclingCapacityTest) {
    size_t idx = 0;
    double tol = 0.001;

    // check capacity degradation with no cycling
    while (idx < 2400) {
        model->runLifetimeModels(idx, false, 50, 50, 25);
        idx++;
    }

    auto state = model->get_state();
    EXPECT_EQ(state.n_cycles, 0);
    EXPECT_NEAR(state.day_age_of_battery, 100, tol);
    EXPECT_NEAR(state.q_relative, 98.825, tol);
    EXPECT_NEAR(state.nmc_state->q_relative_li, 98.825, tol);
    EXPECT_NEAR(state.nmc_state->q_relative_neg, 100, tol);
    EXPECT_NEAR(state.nmc_state->dq_relative_li_old, 0.082, tol);
    EXPECT_EQ(state.nmc_state->b1_dt, 0);
    EXPECT_EQ(state.nmc_state->b2_dt, 0);
    EXPECT_EQ(state.nmc_state->b3_dt, 0);
    EXPECT_EQ(state.nmc_state->c2_dt, 0);

    model = std::unique_ptr<lifetime_nmc_t>(new lifetime_nmc_t(dt_hour));
    idx = 0;

    // simulate at battery temperature 35 C for 300 days
    while (idx < 7200) {
        model->runLifetimeModels(idx, false, 50, 50, 35);
        idx++;
    }

    state = model->get_state();
    EXPECT_EQ(model->get_state().n_cycles, 0);
    EXPECT_NEAR(state.day_age_of_battery, 300, tol);
    EXPECT_NEAR(state.q_relative, 92.831, tol);
    EXPECT_NEAR(state.nmc_state->q_relative_li, 92.831, tol);
    EXPECT_NEAR(state.nmc_state->q_relative_neg, 100, tol);
    EXPECT_NEAR(state.nmc_state->dq_relative_li_old, 0.142, tol);
    EXPECT_EQ(state.nmc_state->b1_dt, 0);
    EXPECT_EQ(state.nmc_state->b2_dt, 0);
    EXPECT_EQ(state.nmc_state->b3_dt, 0);
    EXPECT_EQ(state.nmc_state->c2_dt, 0);
}
