#include <gtest/gtest.h>

#include "lib_battery_capacity.h"
//#include "lib_battery.h"

#include "lib_battery_capacity_test.h"

TEST_F(LiIon_lib_battery_capacity_test, SetUpTest) {
    EXPECT_NEAR(old_cap->q1(), 500, 1e-4);
    EXPECT_NEAR(old_cap->q10(), 1000, 1e-4);
    EXPECT_NEAR(old_cap->SOC(), 50, 1e-4);
}

TEST_F(LiIon_lib_battery_capacity_test, updateCapacityTest){
    double I = 1.5;
    old_cap->updateCapacity(I, dt_hour);
    auto s1 = capacity_state({498.5, 1000, 1000, 1.5, 0,
                             49.85,  50, 2});
    compareState(old_cap->get_state(), s1, "updateCapacityTest: 1");

    I = 3;
    old_cap->updateCapacity(I, dt_hour);

    s1 = {495.5, 1000, 1000, 3, 0,
          49.55, 49.85, 2};
    compareState(old_cap->get_state(), s1, "updateCapacityTest: 2");

    I = 490;
    old_cap->updateCapacity(I, dt_hour);
    s1 = {150, 1000, 1000, 345.5, 0,
          15, 49.55, 2};
    compareState(old_cap->get_state(), s1, "updateCapacityTest: 3");

    I = 490;
    old_cap->updateCapacity(I, dt_hour);
    s1 = {150, 1000, 1000, 0, 0,
          15, 15, 1};
    compareState(old_cap->get_state(), s1, "updateCapacityTest: 4");
}

TEST_F(LiIon_lib_battery_capacity_test,  updateCapacityThermalTest){
    double percent = 80;
    old_cap->updateCapacityForThermal(percent);
    auto s1 = capacity_state({500, 1000, 800, 0, 0,
                              62.5, 50, 2});
    compareState(old_cap->get_state(), s1, "updateCapacityThermalTest: 1");

    percent = 50;
    old_cap->updateCapacityForThermal(percent);
    s1 = {500, 1000, 500, 0, 0, 100, 50, 2};
    compareState(old_cap->get_state(), s1, "updateCapacityThermalTest: 2");

    percent = 10;
    old_cap->updateCapacityForThermal(percent);
    s1 = {100, 1000, 100, 0, 400, 100, 50, 2};
    compareState(old_cap->get_state(), s1, "updateCapacityThermalTest: 3");

    percent = 110;
    old_cap->updateCapacityForThermal(percent);
    s1 = {100, 1000, 1100, 0, 400, 10, 50, 2};
    compareState(old_cap->get_state(), s1, "updateCapacityThermalTest: 4");

    percent = -110;
    old_cap->updateCapacityForThermal(percent);
    s1 = {0, 1000, 0, 0, 500, 0, 50, 2};
    compareState(old_cap->get_state(), s1, "updateCapacityThermalTest: 4");

}

TEST_F(LiIon_lib_battery_capacity_test, updateCapacityLifetimeTest){
    double percent = 80;
    old_cap->updateCapacityForLifetime(percent);
    auto s1 = capacity_state({500, 800, 1000, 0, 0,62.5, 50, 2});
    compareState(old_cap->get_state(), s1, "updateCapacityLifetimeTest: 1");

    percent = 50;
    old_cap->updateCapacityForLifetime(percent);
    s1 = {500, 500, 1000, 0, 0, 100, 50, 2};
    compareState(old_cap->get_state(), s1, "updateCapacityLifetimeTest: 2");

    percent = 10;
    old_cap->updateCapacityForLifetime(percent);
    s1 = {100, 100, 1000, 0, 400, 100, 50, 2};
    compareState(old_cap->get_state(), s1, "updateCapacityLifetimeTest: 3");

    percent = 110;
    old_cap->updateCapacityForLifetime(percent);
    s1 = {100, 100, 1000, 0, 400, 100, 50, 2};
    compareState(old_cap->get_state(), s1, "updateCapacityLifetimeTest: 4");

    percent = -110;
    old_cap->updateCapacityForLifetime(percent);
    s1 = {0, 0, 1000, 0, 500, 0, 50, 2};
    compareState(old_cap->get_state(), s1, "updateCapacityLifetimeTest: 5");
}

TEST_F(LiIon_lib_battery_capacity_test, replaceBatteryTest){
    auto s1 = capacity_state{500, 1000, 1000, 0, 0, 50, 50, 2};
    compareState(old_cap->get_state(), s1, "replaceBatteryTest: init");

    // degrade 100% of battery
    old_cap->updateCapacityForLifetime(0);
    s1 = {0, 0, 1000, 0, 500,0, 50, 2};
    compareState(old_cap->get_state(), s1, "replaceBatteryTest: init degradation");

    double percent = 50;
    old_cap->replace_battery(percent);
    s1 = {250, 500, 500, 0, 500,50, 50, 2};
    compareState(old_cap->get_state(), s1, "replaceBatteryTest: 1");

    percent = 20;
    old_cap->replace_battery(percent);
    s1 = {350, 700, 700, 0, 500, 50, 50, 2};
    compareState(old_cap->get_state(), s1, "replaceBatteryTest: 2");

    percent = 110;
    old_cap->replace_battery(percent);
    s1 = {500, 1000, 1000, 0, 500, 50, 50, 2};
    compareState(old_cap->get_state(), s1, "replaceBatteryTest: 4");

    percent = -110;
    old_cap->replace_battery(percent);
    s1 = {500, 1000, 1000, 0, 500, 50, 50, 2};
    compareState(old_cap->get_state(), s1, "replaceBatteryTest: 5");
}

TEST_F(LiIon_lib_battery_capacity_test, runSequenceTest) {
    double I = 400;
    old_cap->updateCapacity(I, dt_hour);
    auto s1 = capacity_state({150, 1000, 1000, 350, 0,
                              15, 50, 2});
    compareState(old_cap->get_state(), s1, "runSequenceTest: 1");
    I = -400;
    old_cap->updateCapacity(I, dt_hour);
    s1 = capacity_state({550, 1000, 1000, -400, 0,
                              55, 15, 0});
    compareState(old_cap->get_state(), s1, "runSequenceTest: 2");

    double percent = 80;
    old_cap->updateCapacityForThermal(percent);
    s1 = capacity_state({550, 1000, 800, -400, 0,
                         68.75, 15, 0});
    compareState(old_cap->get_state(), s1, "runSequenceTest: 3");

    I = 400;
    old_cap->updateCapacity(I, dt_hour);
    s1 = capacity_state({150, 1000, 800, 400, 0,
                         18.75, 68.75, 2});
    compareState(old_cap->get_state(), s1, "runSequenceTest: 4");

    I = -400;
    old_cap->updateCapacity(I, dt_hour);
    s1 = capacity_state({550, 1000, 800, -400, 0,
                         68.75, 18.75, 0});
    compareState(old_cap->get_state(), s1, "runSequenceTest: 5");

    percent = 70;
    old_cap->updateCapacityForLifetime(percent);
    s1 = capacity_state({550, 700, 800, -400, 0,
                         78.57, 18.75, 0});
    compareState(old_cap->get_state(), s1, "runSequenceTest: 6");

    percent = 20;
    old_cap->replace_battery(percent);
    s1 = {650, 900, 900, -400, 0, 72.22, 50, 0};
    compareState(old_cap->get_state(), s1, "replaceBatteryTest: 7");

    I = 400;
    old_cap->updateCapacity(I, dt_hour);
    s1 = capacity_state({250, 900, 900, 400, 0,
                         27.77, 72.22, 2});
    compareState(old_cap->get_state(), s1, "replaceBatteryTest: 8");
}

// battery is overcharged so capacity gets reduced while we want to discharge some current
TEST_F(LiIon_lib_battery_capacity_test, OverchargedSOCLimits) {
    double I = 35.;
    double dt = 1./60.;

    auto cap = std::make_shared<capacity_lithium_ion_t>(19984, 90, SOC_max, SOC_min, dt);
    cap->updateCapacityForThermal(90);

    // charge will be dumped
    cap->updateCapacity(I, dt);

    ASSERT_EQ(I, 35);
}

// battery is overcharged so capacity gets reduced while we want to charge some current
TEST_F(LiIon_lib_battery_capacity_test, OverchargedSOCLimits1) {
    double I = -20.;
    double dt = 1./60.;

    auto cap = std::make_shared<capacity_lithium_ion_t>(19984, 90, SOC_max, SOC_min, dt);
    cap->updateCapacityForThermal(90);

    // charge will be dumped
    cap->updateCapacity(I, dt);

    ASSERT_NEAR(I, 0, 1e-2);
    ASSERT_NEAR(cap->SOC(), SOC_max, 1e-3);
}

// battery is undercharged so capacity gets increased while we want to discharge some current
TEST_F(LiIon_lib_battery_capacity_test, UnderchargedSOCLimits) {
    double I = 35.;
    double dt = 1./60.;

    auto cap = std::make_shared<capacity_lithium_ion_t>(19984, 10, SOC_max, SOC_min, dt);
    cap->updateCapacityForThermal(90);

    cap->updateCapacity(I, dt);

    ASSERT_EQ(I, 0);
}

// battery is undercharged so capacity gets increased while we want to charge some current
TEST_F(LiIon_lib_battery_capacity_test, UnderchargedSOCLimits1) {
    double I = -35.;
    double dt = 1./60.;

    auto cap = std::make_shared<capacity_lithium_ion_t>(19984, 10, SOC_max, SOC_min, dt);
    cap->updateCapacityForThermal(90);
    cap->updateCapacity(I, dt);

    ASSERT_EQ(I, -35);
}

TEST_F(KiBam_lib_battery_capacity_test, SetUpTest) {
    EXPECT_NEAR(old_cap->q1(), 25.6938, tol);
    EXPECT_EQ(old_cap->q10(), 93);
    EXPECT_NEAR(old_cap->qmax(), 25.6938, 108.15);
}

TEST_F(KiBam_lib_battery_capacity_test, updateCapacityTest){
    double I = 1.5;
    old_cap->updateCapacity(I, dt_hour);
    auto s1 = capacity_state({52.58, 108.16, 108.16, 1.5, 0,
                              48.613, 50, 2});
    compareState(old_cap->get_state(), s1, "updateCapacityTest: 1");

    I = 3;
    old_cap->updateCapacity(I, dt_hour);

    s1 = {49.58, 108.16, 108.16, 3, 0,
          45.839, 48.613, 2};
    compareState(old_cap->get_state(), s1, "updateCapacityTest: 2");

    I = 490;
    old_cap->updateCapacity(I, dt_hour);
    s1 = {22.927, 108.16, 108.16, 26.65, 0,
          21.19, 45.839, 2};
    compareState(old_cap->get_state(), s1, "updateCapacityTest: 3");

    I = 490;
    old_cap->updateCapacity(I, dt_hour);
    s1 = {16.67, 108.16, 108.16, 6.25, 0,
          15.413, 21.19, 2};
    compareState(old_cap->get_state(), s1, "updateCapacityTest: 4");
}


TEST_F(KiBam_lib_battery_capacity_test, updateCapacityThermalTest){
    double percent = 80;
    old_cap->updateCapacityForThermal(percent);
    auto s1 = capacity_state({54.07, 108.15, 86.53, 0, 0,
                              62.5, 50, 2});
    compareState(old_cap->get_state(), s1, "updateCapacityThermalTest: 1");

    percent = 50;
    old_cap->updateCapacityForThermal(percent);
    s1 = {54.07, 108.15, 54.07, 0, 0, 100, 50, 2};
    compareState(old_cap->get_state(), s1, "updateCapacityThermalTest: 2");

    percent = 10;
    old_cap->updateCapacityForThermal(percent);
    s1 = {10.816, 108.15, 10.816, 0, 43.26, 100, 50, 2};
    compareState(old_cap->get_state(), s1, "updateCapacityThermalTest: 3");

    percent = 110;
    old_cap->updateCapacityForThermal(percent);
    s1 = {10.816, 108.15, 118.97, 0, 43.26, 10, 50, 2};
    compareState(old_cap->get_state(), s1, "updateCapacityThermalTest: 4");

    percent = -110;
    old_cap->updateCapacityForThermal(percent);
    s1 = {0, 108.15, 0, 0, 54.07, 0, 50, 2};
    compareState(old_cap->get_state(), s1, "updateCapacityThermalTest: 4");

}

TEST_F(KiBam_lib_battery_capacity_test, updateCapacityLifetimeTest){
    double percent = 80;
    old_cap->updateCapacityForLifetime(percent);
    auto s1 = capacity_state({54.07, 86.53, 108.15, 0, 0,62.5, 50, 2});
    compareState(old_cap->get_state(), s1, "updateCapacityLifetimeTest: 1");

    percent = 50;
    old_cap->updateCapacityForLifetime(percent);
    s1 = {54.07, 54.07, 108.15, 0, 0, 100, 50, 2};
    compareState(old_cap->get_state(), s1, "updateCapacityLifetimeTest: 2");

    percent = 10;
    old_cap->updateCapacityForLifetime(percent);
    s1 = {10.816, 10.816, 108.15, 0, 43.26, 100, 50, 2};
    compareState(old_cap->get_state(), s1, "updateCapacityLifetimeTest: 3");

    percent = 110;
    old_cap->updateCapacityForLifetime(percent);
    s1 = {10.816, 10.816, 108.15, 0, 43.26, 100, 50, 2};
    compareState(old_cap->get_state(), s1, "updateCapacityLifetimeTest: 4");

    percent = -110;
    old_cap->updateCapacityForLifetime(percent);
    s1 = {0, 0, 108.15, 0, 54.07, 0, 50, 2};
    compareState(old_cap->get_state(), s1, "updateCapacityLifetimeTest: 5");
}

TEST_F(KiBam_lib_battery_capacity_test, replaceBatteryTest){
    auto s1 = capacity_state{54.07, 108.15, 108.15, 0, 0, 50, 50, 2};
    compareState(old_cap->get_state(), s1, "replaceBatteryTest: init");

    // degrade 100% of battery
    old_cap->updateCapacityForLifetime(0);
    s1 = {0, 0, 108.15, 0, 54.07,0, 50, 2};
    compareState(old_cap->get_state(), s1, "replaceBatteryTest: init degradation");

    double percent = 50;
    old_cap->replace_battery(percent);
    s1 = {27.04, 54.07, 54.07, 0, 54.07,50, 50, 2};
    compareState(old_cap->get_state(), s1, "replaceBatteryTest: 1");

    percent = 20;
    old_cap->replace_battery(percent);
    s1 = {37.85, 75.71, 75.71, 0, 54.07, 50, 50, 2};
    compareState(old_cap->get_state(), s1, "replaceBatteryTest: 2");

    percent = 110;
    old_cap->replace_battery(percent);
    s1 = {54.07, 108.15, 108.15, 0, 54.07, 50, 50, 2};
    compareState(old_cap->get_state(), s1, "replaceBatteryTest: 4");

    percent = -110;
    old_cap->replace_battery(percent);
    s1 = {54.07, 108.15, 108.15, 0, 54.07, 50, 50, 2};
    compareState(old_cap->get_state(), s1, "replaceBatteryTest: 5");
}

TEST_F(KiBam_lib_battery_capacity_test, runSequenceTest) {
    double I = 30;
    old_cap->updateCapacity(I, dt_hour);
    auto s1 = capacity_state({24.07, 108.16, 108.16, 30, 0,
                              22.26, 50, 2});
    compareState(old_cap->get_state(), s1, "runSequenceTest: 1");
    I = -30;
    old_cap->updateCapacity(I, dt_hour);
    s1 = capacity_state({54.07, 108.16, 108.16, -30, 0,
                         50, 22.26, 0});
    compareState(old_cap->get_state(), s1, "runSequenceTest: 2");

    double percent = 80;
    old_cap->updateCapacityForThermal(percent);
    s1 = capacity_state({54.07, 108.16, 86.53, -30, 0,
                         62.5, 22.26, 0});
    compareState(old_cap->get_state(), s1, "runSequenceTest: 3");

    I = 40;
    old_cap->updateCapacity(I, dt_hour);
    s1 = capacity_state({20.74, 108.16, 86.53, 33.34, 0,
                         23.97, 62.5, 2});
    compareState(old_cap->get_state(), s1, "runSequenceTest: 4");

    I = -40;
    old_cap->updateCapacity(I, dt_hour);
    s1 = capacity_state({60.74, 108.16, 86.53, -40, 0,
                         70.19, 23.97, 0});
    compareState(old_cap->get_state(), s1, "runSequenceTest: 5");

    percent = 70;
    old_cap->updateCapacityForLifetime(percent);
    s1 = capacity_state({60.74, 75.71, 86.53, -40, 0,
                         80.22, 23.97, 0});
    compareState(old_cap->get_state(), s1, "runSequenceTest: 6");

    percent = 20;
    old_cap->replace_battery(percent);
    s1 = {71.55, 97.34, 97.34, -40, 0, 73.5, 50, 0};
    compareState(old_cap->get_state(), s1, "replaceBatteryTest: 7");

    I = 40;
    old_cap->updateCapacity(I, dt_hour);
    s1 = capacity_state({31.86, 97.34, 97.34, 39.7, 0,
                         32.73, 73.5, 2});
    compareState(old_cap->get_state(), s1, "replaceBatteryTest: 8");
}
