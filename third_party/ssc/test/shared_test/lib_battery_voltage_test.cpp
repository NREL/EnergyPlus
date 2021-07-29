#include <gtest/gtest.h>
#include <cmath>

//#include "lib_battery_capacity.h"
#include "lib_battery.h"
#include "lib_battery_voltage_test.h"

TEST_F(voltage_dynamic_lib_battery_voltage_test, SetUpTest) {
    CreateModel(1);

    EXPECT_NEAR(model->cell_voltage(), 4.058, 1e-3);
}

TEST_F(voltage_dynamic_lib_battery_voltage_test, NickelMetalHydrideFromPaperTest){
    CreateModel(1);

    // Figure 3 from A Generic Battery Model for the Dynamic Simulation of Hybrid Electric Vehicles
    cap = std::unique_ptr<capacity_lithium_ion_t>(new capacity_lithium_ion_t(6.5, 100, 100, 0, 1));

    model = std::unique_ptr<voltage_t>(new voltage_dynamic_t(1, 1, 1.2, 1.4,
                                                             1.25, 1.2, 6.5, 1.3, 5.2, 0.2, 0.0046, 1));
    std::vector<double> dt_hr = {1./6., 1./3., 1./3.};
    // testing with 1lt curve
    std::vector<double> voltages = {1.25, 1.22, 1.17};
    for (size_t i = 0; i < 3; i++){
        double I = 6.5;
        cap->updateCapacity(I, dt_hr[i]);
        model->updateVoltage(cap->q0(), cap->qmax(), cap->I(), 0, dt_hr[i]);
        EXPECT_NEAR(model->battery_voltage(), voltages[i], 0.05) << "NickelMetalHydrideFromPaperTest: " + std::to_string(i);
    }
}

TEST_F(voltage_dynamic_lib_battery_voltage_test, updateCapacityTest){
    double dt_hour = 1;
    CreateModel(dt_hour);

    double I = 2;
    cap->updateCapacity(I, dt_hour); // qmx = 10, q0 = 3
    model->updateVoltage(cap->q0(), cap->qmax(), cap->I(), 0, dt_hour);
    EXPECT_NEAR(model->cell_voltage(), 3.9, tol);
    EXPECT_NEAR(cap->q0(), 3, tol);

    I = -2;
    cap->updateCapacity(I, dt_hour); // qmx = 10, q0 = 5
    model->updateVoltage(cap->q0(), cap->qmax(), cap->I(), 0, dt_hour);
    EXPECT_NEAR(model->cell_voltage(), 4.1, tol);
    EXPECT_NEAR(cap->q0(), 5, tol);

    I = 5;
    cap->updateCapacity(I, dt_hour); // qmx = 10, I = 4.5, q0 = 0.5
    model->updateVoltage(cap->q0(), cap->qmax(), cap->I(), 0, dt_hour);
    EXPECT_NEAR(model->cell_voltage(), 2.49, tol);
    EXPECT_NEAR(cap->q0(), 0.5, tol);

}

TEST_F(voltage_dynamic_lib_battery_voltage_test, updateCapacitySubHourly){
    double dt_hour = 1. / 2;
    CreateModel(dt_hour);

    double I = 2;
    cap->updateCapacity(I, dt_hour); // qmx = 10, q0 = 3
    model->updateVoltage(cap->q0(), cap->qmax(), cap->I(), 0, dt_hour);
    EXPECT_NEAR(model->cell_voltage(), 3.98, tol);
    EXPECT_NEAR(cap->q0(), 4, tol);

    I = -2;
    cap->updateCapacity(I, dt_hour); // qmx = 10, q0 = 5
    model->updateVoltage(cap->q0(), cap->qmax(), cap->I(), 0, dt_hour);
    EXPECT_NEAR(model->cell_voltage(), 4.1, tol);
    EXPECT_NEAR(cap->q0(), 5, tol);

    I = 5;
    cap->updateCapacity(I, dt_hour); // qmx = 10, I = 4.5, q0 = 0.5
    model->updateVoltage(cap->q0(), cap->qmax(), cap->I(), 0, dt_hour);
    EXPECT_NEAR(model->cell_voltage(), 3.78, tol);
    EXPECT_NEAR(cap->q0(), 2.5, tol);

}

TEST_F(voltage_dynamic_lib_battery_voltage_test, updateCapacitySubMinute){
    double dt_hour = 1. / 200;
    CreateModel(dt_hour);

    double I = 2;
    cap->updateCapacity(I, dt_hour); // qmx = 10, q0 = 3
    model->updateVoltage(cap->q0(), cap->qmax(), cap->I(), 0, dt_hour);
    EXPECT_NEAR(model->cell_voltage(), 4.014, 1e-3);
    EXPECT_NEAR(cap->q0(), 4.99, 1e-3);

    I = -2;
    cap->updateCapacity(I, dt_hour); // qmx = 10, q0 = 5
    model->updateVoltage(cap->q0(), cap->qmax(), cap->I(), 0, dt_hour);
    EXPECT_NEAR(model->cell_voltage(), 4.103, 1e-3);
    EXPECT_NEAR(cap->q0(), 5, 1e-3);

    I = 5;
    cap->updateCapacity(I, dt_hour); // qmx = 10, I = 4.5, q0 = 0.5
    model->updateVoltage(cap->q0(), cap->qmax(), cap->I(), 0, dt_hour);
    EXPECT_NEAR(model->cell_voltage(), 3.947, 1e-3);
    EXPECT_NEAR(cap->q0(), 4.975, 1e-3);
}

TEST_F(voltage_dynamic_lib_battery_voltage_test, calculateMaxChargeHourly){
    double dt_hour = 1;
    CreateModel(dt_hour);

    // start at half SOC
    double max_current;
    double power = model->calculate_max_charge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, -2989, 1);
    double max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, max_current, 1e-3);
    // max current reduced to enforce SOC
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 95, 1e-3);


    // start at full SOC
    power = model->calculate_max_charge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, -292, 1);
    max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, max_current, 1e-3);
    // max current reduced to enforce SOC
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 95, 1e-3);


    // start at empty SOC
    double I = 2;
    while (cap->SOC() > 5)
        cap->updateCapacity(I, dt_hour);
    power = model->calculate_max_charge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, -5811, 1);
    max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, max_current, 1e-3);
    // max current reduced to enforce SOC
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 95, 1e-3);
}

TEST_F(voltage_dynamic_lib_battery_voltage_test, calculateMaxChargeSubHourly){
    double dt_hour = 0.5;
    CreateModel(dt_hour);

    // start at half SOC
    double max_current;
    double power = model->calculate_max_charge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, -6132, 1);
    double max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, max_current, 1e-3);
    // max current reduced to enforce SOC
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 95, 1e-3);

    // start at full SOC
    power = model->calculate_max_charge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, -585, 1);
    max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, max_current, 1e-3);
    // max current reduced to enforce SOC
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 95, 1e-3);


    // start at empty SOC
    double I = 2;
    while (cap->SOC() > 5)
        cap->updateCapacity(I, dt_hour);
    power = model->calculate_max_charge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, -12180, 1);
    max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, max_current, 1e-3);
    // max current reduced to enforce SOC
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 95, 1e-3);
}

TEST_F(voltage_dynamic_lib_battery_voltage_test, calculateMaxChargeSubMinute){
    double dt_hour = 1. / 360;
    CreateModel(dt_hour);

    // start at half SOC
    double max_current;
    double power = model->calculate_max_charge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, -11056338, 1);
    double max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, max_current, 1e-3);
    // max current reduced to enforce SOC
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 95, 1e-3);

    // start at full SOC
    power = model->calculate_max_charge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, -204913, 1);
    max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, max_current, 1e-3);
    // max current reduced to enforce SOC
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 95, 1e-3);


    // start at empty SOC
    double I = 2;
    while (cap->SOC() > 5)
        cap->updateCapacity(I, dt_hour);
    power = model->calculate_max_charge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, -38120722, 1);
    max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, max_current, 1e-3);
    // max current reduced to enforce SOC
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 95, 1e-3);
}

TEST_F(voltage_dynamic_lib_battery_voltage_test, calculateMaxDischargeHourly){
    double dt_hour = 1;
    CreateModel(dt_hour);

    // start at half SOC
    double max_current;
    double power = model->calculate_max_discharge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, 1845, 1);        // current ~4
    double max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, max_current, 1e-2);
    // Does not empty battery for highest power
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 10, 1e-3);

    // start at empty SOC
    power = model->calculate_max_discharge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, 181, 1);
    max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, max_current, 1e-1);
    // Empties battery for highest power
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 5, 1e-3);

    // start at full SOC
    double I = -2;
    while (cap->SOC() < 95)
        cap->updateCapacity(I, dt_hour);
    power = model->calculate_max_discharge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, 3829, 1);
    max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, max_current, 1e-2);
    // Does not empty battery for highest power
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 19, 1e-3);
}

TEST_F(voltage_dynamic_lib_battery_voltage_test, calculateMaxDischargeSubHourly){
    double dt_hour = 0.5;
    CreateModel(dt_hour);

    // start at half SOC
    double max_current;
    double power = model->calculate_max_discharge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, 3592, 1);        // current ~8
    double max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, max_current, 0.2);
    // Does not empty battery for highest power
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 10, 1e-3);

    // start at empty SOC
    power = model->calculate_max_discharge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, 365, 1);
    max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, max_current, 1e-1);
    // Empties battery for highest power
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 5, 1e-3);

    // start at full SOC
    double I = -2;
    while (cap->SOC() < 95)
        cap->updateCapacity(I, dt_hour);
    power = model->calculate_max_discharge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, 7390, 1);
    max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, max_current, 0.3);
    // Does not empty battery for highest power
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 14.25, 1e-3);
}

TEST_F(voltage_dynamic_lib_battery_voltage_test, calculateMaxDischargeSubMinute){
    double dt_hour = 1. / 200;
    CreateModel(dt_hour);

    // start at half SOC
    double max_current;
    double power = model->calculate_max_discharge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, 25554, 1);        // current ~8
    double max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, max_current, 0.2);
    // Does not empty battery for highest power
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 45.475, 1e-3);

    // start at empty SOC
    power = model->calculate_max_discharge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, 25307, 1);
    max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, max_current, 1e-1);
    // Does not empty battery for highest power
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 40.973, 1e-3);

    // start at full SOC
    double I = -2;
    while (cap->SOC() < 95)
        cap->updateCapacity(I, dt_hour);
    power = model->calculate_max_discharge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, 26689, 1);
    max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, max_current, 0.6);
    // Does not empty battery for highest power
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 90.345, 1e-3);
}

TEST_F(voltage_table_lib_battery_voltage_test, updateCapacityTest){
    double dt_hour = 1;
    CreateModel(dt_hour);

    double I = 2;
    cap->updateCapacity(I, dt_hour); // qmx = 10, q0 = 3
    model->updateVoltage(cap->q0(), cap->qmax(), cap->I(), 0, dt_hour);
    EXPECT_NEAR(model->cell_voltage(), 3.54, tol);
    EXPECT_NEAR(cap->q0(), 3, tol);

    I = -2;
    cap->updateCapacity(I, dt_hour); // qmx = 10, q0 = 5
    model->updateVoltage(cap->q0(), cap->qmax(), cap->I(), 0, dt_hour);
    EXPECT_NEAR(model->cell_voltage(), 3.7, tol);
    EXPECT_NEAR(cap->q0(), 5, tol);

    I = 5;
    cap->updateCapacity(I, dt_hour); // qmx = 10, I = 4.5, q0 = 0.5
    model->updateVoltage(cap->q0(), cap->qmax(), cap->I(), 0, dt_hour);
    EXPECT_NEAR(model->cell_voltage(), 1.35, tol);
    EXPECT_NEAR(cap->q0(), 0.5, tol);
}

TEST_F(voltage_table_lib_battery_voltage_test, updateCapacitySubHourly){
    double dt_hour = 1. / 2;
    CreateModel(dt_hour);

    double I = 2;
    cap->updateCapacity(I, dt_hour); // qmx = 10, q0 = 3
    model->updateVoltage(cap->q0(), cap->qmax(), cap->I(), 0, dt_hour);
    EXPECT_NEAR(model->cell_voltage(), 3.616, tol);
    EXPECT_NEAR(cap->q0(), 4, tol);

    I = -2;
    cap->updateCapacity(I, dt_hour); // qmx = 10, q0 = 5
    model->updateVoltage(cap->q0(), cap->qmax(), cap->I(), 0, dt_hour);
    EXPECT_NEAR(model->cell_voltage(), 3.7, tol);
    EXPECT_NEAR(cap->q0(), 5, tol);

    I = 5;
    cap->updateCapacity(I, dt_hour); // qmx = 10, I = 4.5, q0 = 0.5
    model->updateVoltage(cap->q0(), cap->qmax(), cap->I(), 0, dt_hour);
    EXPECT_NEAR(model->cell_voltage(), 3.504, tol);
    EXPECT_NEAR(cap->q0(), 2.5, tol);
}

TEST_F(voltage_table_lib_battery_voltage_test, updateCapacitySubMinute){
    double dt_hour = 1. / 200;
    CreateModel(dt_hour);

    double I = 2;
    cap->updateCapacity(I, dt_hour); // qmx = 10, q0 = 3
    model->updateVoltage(cap->q0(), cap->qmax(), cap->I(), 0, dt_hour);
    EXPECT_NEAR(model->cell_voltage(), 3.689, tol);
    EXPECT_NEAR(cap->q0(), 4.99, 1e-3);

    I = -2;
    cap->updateCapacity(I, dt_hour); // qmx = 10, q0 = 5
    model->updateVoltage(cap->q0(), cap->qmax(), cap->I(), 0, dt_hour);
    EXPECT_NEAR(model->cell_voltage(), 3.69, tol);
    EXPECT_NEAR(cap->q0(), 5, 1e-3);

    I = 5;
    cap->updateCapacity(I, dt_hour); // qmx = 10, I = 4.5, q0 = 0.5
    model->updateVoltage(cap->q0(), cap->qmax(), cap->I(), 0, dt_hour);
    EXPECT_NEAR(model->cell_voltage(), 3.689, tol);
    EXPECT_NEAR(cap->q0(), 4.975, 1e-3);
}

TEST_F(voltage_table_lib_battery_voltage_test, calculateMaxChargeHourly1){
    double dt_hour = 1;
    CreateModel(dt_hour);
    cap->change_SOC_limits(0, 100);

    // start at half SOC
    double max_current;
    double power = model->calculate_max_charge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, -2849, 1);

    // check current estimated for max power
    double max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, -5, 1e-3);
    cap->updateCapacity(max_current_calc, dt_hour);
    EXPECT_NEAR(cap->SOC(), 100, 1e-3);

    // re-discharge to 50 SOC
    max_current_calc *= -1;
    cap->updateCapacity(max_current_calc, dt_hour);

    // little less than max power
    max_current_calc = model->calculate_current_for_target_w(power + 1, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, -4.99, 1e-2);
    cap->updateCapacity(max_current_calc, dt_hour);
    EXPECT_NEAR(cap->SOC(), 99.99, 1e-2);
    EXPECT_NEAR(cap->I() * model->battery_voltage(), -2564, 1);
}

TEST_F(voltage_table_lib_battery_voltage_test, calculateMaxChargeHourly2){
    double dt_hour = 1;
    CreateModel(dt_hour);
    cap->change_SOC_limits(0, 100);

    // start at 90 SOC
    double max_current = -4;
    cap->updateCapacity(max_current, dt_hour);

    double power = model->calculate_max_charge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, -569, 1);

    double max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, -1, 1e-3);

    cap->updateCapacity(max_current_calc, dt_hour);
    EXPECT_NEAR(cap->SOC(), 100, 1e-3);

    // re-discharge to 90 SOC
    max_current_calc = 1;
    cap->updateCapacity(max_current_calc, dt_hour);

    // little less than max power
    power = model->calculate_max_charge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, -569, 1);

    max_current_calc = model->calculate_current_for_target_w(power + 1, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, -0.998, 1e-3);

    // max current reduced to enforce SOC
    cap->updateCapacity(max_current_calc, dt_hour);
    EXPECT_NEAR(cap->SOC(), 99.984, 1e-3);
    EXPECT_NEAR(cap->I() * model->battery_voltage(), -512, 1);

    // start at empty SOC
    double I = 2;
    while (cap->SOC() > 5)
        cap->updateCapacity(I, dt_hour);
    power = model->calculate_max_charge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, -5699, 1);
    max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, max_current, 1e-3);
    // max current reduced to enforce SOC
    cap->updateCapacity(max_current_calc, dt_hour);
    EXPECT_NEAR(cap->SOC(), 100, 1e-3);
}

TEST_F(voltage_table_lib_battery_voltage_test, calculateMaxChargeHourly3){
    double dt_hour = 1;
    CreateModel(dt_hour);
    cap->change_SOC_limits(0, 100);

    // start at 10 SOC
    double max_current = 4;
    cap->updateCapacity(max_current, dt_hour);

    double power = model->calculate_max_charge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, -5129, 1);

    double max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, -9, 1e-3);

    cap->updateCapacity(max_current_calc, dt_hour);
    EXPECT_NEAR(cap->SOC(), 100, 1e-3);

    // re-charge to 10 SOC
    max_current_calc = 9;
    cap->updateCapacity(max_current_calc, dt_hour);

    // little less than max power
    power = model->calculate_max_charge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, -5129, 1);

    max_current_calc = model->calculate_current_for_target_w(power + 1, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, -8.99, 1e-2);

    // max current reduced to enforce SOC
    cap->updateCapacity(max_current_calc, dt_hour);
    EXPECT_NEAR(cap->SOC(), 99.99, 1e-2);
    EXPECT_NEAR(cap->I() * model->battery_voltage(), -4615, 1);
}

TEST_F(voltage_table_lib_battery_voltage_test, calculateMaxChargeSubHourly1){
    double dt_hour = .5;
    CreateModel(dt_hour);
    cap->change_SOC_limits(0, 100);

    // start at half SOC
    double max_current;
    double power = model->calculate_max_charge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, -5699, 1);

    // check current estimated for max power
    double max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, -10, 1e-3);
    // max current reduced to enforce SOC
    cap->updateCapacity(max_current_calc, dt_hour);
    EXPECT_NEAR(cap->SOC(), 100, 1e-3);

    // re-discharge to 50 SOC
    max_current_calc *= -1;
    cap->updateCapacity(max_current_calc, dt_hour);

    // little less than max power
    max_current_calc = model->calculate_current_for_target_w(power + 1, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, -9.999, 1e-3);
    // max current reduced to enforce SOC
    cap->updateCapacity(max_current_calc, dt_hour);
    EXPECT_NEAR(cap->SOC(), 99.99, 1e-2);
    EXPECT_NEAR(cap->I() * model->battery_voltage(), -5128, 1);
}

TEST_F(voltage_table_lib_battery_voltage_test, calculateMaxChargeSubHourly2){
    double dt_hour = 0.5;
    CreateModel(dt_hour);
    cap->change_SOC_limits(0, 100);

    // start at 90 SOC
    double max_current = -8;
    cap->updateCapacity(max_current, dt_hour);

    double power = model->calculate_max_charge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, -1139, 1);

    double max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, -2, 1e-3);

    cap->updateCapacity(max_current_calc, dt_hour);
    EXPECT_NEAR(cap->SOC(), 100, 1e-3);

    // re-discharge to 90 SOC
    max_current_calc = 2;
    cap->updateCapacity(max_current_calc, dt_hour);

    // little less than max power
    power = model->calculate_max_charge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, -1139, 1);

    max_current_calc = model->calculate_current_for_target_w(power + 1, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, -2, 1e-2);

    cap->updateCapacity(max_current_calc, dt_hour);
    EXPECT_NEAR(cap->SOC(), 99.99, 1e-2);
    EXPECT_NEAR(cap->I() * model->battery_voltage(), -1025, 1);

    // start at empty SOC
    double I = 2;
    while (cap->SOC() > 5)
        cap->updateCapacity(I, dt_hour);
    power = model->calculate_max_charge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, -11398, 1);
    max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, max_current, 1e-3);
    cap->updateCapacity(max_current_calc, dt_hour);
    EXPECT_NEAR(cap->SOC(), 100, 1e-3);
}

TEST_F(voltage_table_lib_battery_voltage_test, calculateMaxChargeSubMinute){
    double dt_hour = 1. / 120;
    CreateModel(dt_hour);

    // start at half SOC
    double max_current;
    double power = model->calculate_max_charge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, -341940, 1);
    double max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, max_current, 1e-3);
    // max current reduced to enforce SOC
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 95, 1e-3);

    // start at full SOC
    power = model->calculate_max_charge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, -34193, 1);
    max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, max_current, 1e-3);
    // max current reduced to enforce SOC
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 95, 1e-3);


    // start at empty SOC
    double I = 2;
    while (cap->SOC() > 5)
        cap->updateCapacity(I, dt_hour);
    power = model->calculate_max_charge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, -649686, 1);
    max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, max_current, 1e-3);
    // max current reduced to enforce SOC
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 95, 1e-3);
}

TEST_F(voltage_table_lib_battery_voltage_test, calculateMaxDischargeHourly){
    double dt_hour = 1;
    CreateModel(dt_hour);
    cap->change_SOC_limits(0, 100);

    // start at half SOC
    double max_current;
    double power = model->calculate_max_discharge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, 1194, 1);        // current ~4
    double max_current_calc = model->calculate_current_for_target_w(power - 1, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, 2.45, 1e-2);
    // Does not empty battery for highest power
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 25.5, 1e-3);

    // start at empty SOC
    power = model->calculate_max_discharge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, 581, 1);
    max_current_calc = model->calculate_current_for_target_w(power - 1, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, 1.22, 1e-1);
    // Empties battery for highest power
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 13.25, 1e-3);

    // start at full SOC
    double I = -2;
    while (cap->SOC() < 95)
        cap->updateCapacity(I, dt_hour);
    power = model->calculate_max_discharge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, 3569, 1);
    max_current_calc = model->calculate_current_for_target_w(power - 1, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, max_current, 1e-2);
    // Does not empty battery for highest power
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 27.02, 1e-2);
}

TEST_F(voltage_table_lib_battery_voltage_test, calculateMaxDischargeSubHourly){
    double dt_hour = 0.5;
    CreateModel(dt_hour);
    cap->change_SOC_limits(0, 100);

    // start at half SOC
    double max_current;
    double power = model->calculate_max_discharge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, 2388, 1);        // current ~4
    double max_current_calc = model->calculate_current_for_target_w(power - 1, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, 4.9, 1e-2);
    // Does not empty battery for highest power
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 25.5, 1e-3);

    // start at empty SOC
    power = model->calculate_max_discharge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, 1163, 1);
    max_current_calc = model->calculate_current_for_target_w(power - 1, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, 2.44, 1e-1);
    // Empties battery for highest power
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 13.25, 1e-3);

    // start at full SOC
    double I = -2;
    while (cap->SOC() < 95)
        cap->updateCapacity(I, dt_hour);
    power = model->calculate_max_discharge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, 7139, 1);
    max_current_calc = model->calculate_current_for_target_w(power - 1, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, max_current, 1e-2);
    // Does not empty battery for highest power
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 27.02, 1e-2);
}

TEST_F(voltage_table_lib_battery_voltage_test, calculateMaxDischargeHourly_table_2) {
    double dt_hour = 1;
    CreateModel_SSC_412(dt_hour);

    // start at half SOC
    double max_current;
    double power = model->calculate_max_discharge_w(cap->q0(), cap->qmax(), 293, &max_current);
    EXPECT_NEAR(power, 3618.3, 1);        // current ~4
    double max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 293);
    EXPECT_NEAR(max_current_calc, 52.06, 1e-2);
    // Does not empty battery for highest power
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 5, 1e-3);

    // start at empty SOC
    power = model->calculate_max_discharge_w(cap->q0(), cap->qmax(), 293, &max_current);
    EXPECT_NEAR(power, 3461.9, 1);
    max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 293);
    EXPECT_NEAR(max_current_calc, max_current, 1e-1);
    // Empties battery for highest power
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 5, 1e-3);

    // start at full SOC
    double I = -2;
    while (cap->SOC() < 95)
        cap->updateCapacity(I, dt_hour);
    power = model->calculate_max_discharge_w(cap->q0(), cap->qmax(), 293, &max_current);
    EXPECT_NEAR(power, 3774.7, 1);
    max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 293);
    EXPECT_NEAR(max_current_calc, 54.31, 1e-2);
    // Does not empty battery for highest power
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 5, 1e-3);
}

TEST_F(voltage_table_lib_battery_voltage_test, calculateMaxDischargeSubMinute){
    double dt_hour = 1. / 200;
    CreateModel(dt_hour);

    // start at half SOC
    double max_current;
    double power = model->calculate_max_discharge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, 238891, 1);        // current ~8
    double max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, max_current, 0.2);
    // Does not empty battery for highest power
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 25.5, 1e-3);

    // start at empty SOC
    power = model->calculate_max_discharge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, 116333, 1);
    max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, max_current, 1e-1);
    // Does not empty battery for highest power
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 13.25, 1e-3);

    // start at full SOC
    double I = -2;
    while (cap->SOC() < 95)
        cap->updateCapacity(I, dt_hour);
    power = model->calculate_max_discharge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, 685795, 1);
    max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 0);
    EXPECT_NEAR(max_current_calc, max_current, 0.3);
    // Does not empty battery for highest power
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 24.52, 1e-3);
}

TEST_F(voltage_table_lib_battery_voltage_test, calculate_discharging_past_limits) {
    double dt_hour = 1;
    CreateModel_SSC_412(dt_hour);

    // start at half SOC
    double max_current;
    double power = model->calculate_max_discharge_w(cap->q0(), cap->qmax(), 293, &max_current);
    EXPECT_NEAR(power, 3618.3, 1);        // current ~4
    double max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 293);
    EXPECT_NEAR(max_current_calc, 52.06, 1e-2);

    // Empty battery somewhat
    double I = 2;
    while (cap->SOC() > 8)
        cap->updateCapacity(I, dt_hour);

    // Estimate too much power, should get low positive current
    max_current_calc = model->calculate_current_for_target_w(1000.0, cap->q0(), cap->qmax(), 293);
    EXPECT_NEAR(max_current_calc, 0.5, 1e-2);

    // Does not empty battery for highest power
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 5, 1e-3);
}


TEST_F(voltage_vanadium_lib_battery_voltage_test, SetUpTest) {
    CreateModel(1);

    EXPECT_EQ(model->cell_voltage(), 3.6);
}

TEST_F(voltage_vanadium_lib_battery_voltage_test, updateCapacityTest){
    double dt_hour = 1;
    CreateModel(dt_hour);

    double I = 2;
    cap->updateCapacity(I, dt_hour); // qmx = 10, q0 = 3
    model->updateVoltage(cap->q0(), cap->qmax(), cap->I(), 293, dt_hour);
    EXPECT_NEAR(model->cell_voltage(), 3.53, tol);
    EXPECT_NEAR(cap->q0(), 3, tol);

    I = -2;
    cap->updateCapacity(I, dt_hour); // qmx = 10, q0 = 5
    model->updateVoltage(cap->q0(), cap->qmax(), cap->I(), 293, dt_hour);
    EXPECT_NEAR(model->cell_voltage(), 3.64, tol);
    EXPECT_NEAR(cap->q0(), 5, tol);

    I = 5;
    cap->updateCapacity(I, dt_hour); // qmx = 10, I = 4.5, q0 = 0.5
    model->updateVoltage(cap->q0(), cap->qmax(), cap->I(), 293, dt_hour);
    EXPECT_NEAR(model->cell_voltage(), 3.3, tol);
    EXPECT_NEAR(cap->q0(), 0.5, tol);
}

TEST_F(voltage_vanadium_lib_battery_voltage_test, updateCapacitySubMinute){
    double dt_hour = 1. / 200;
    CreateModel(dt_hour);

    double I = 2;
    cap->updateCapacity(I, dt_hour); // qmx = 10, q0 = 3
    model->updateVoltage(cap->q0(), cap->qmax(), cap->I(), 293, dt_hour);
    EXPECT_NEAR(model->cell_voltage(), 3.644, tol);
    EXPECT_NEAR(cap->q0(), 4.99, 1e-3);

    I = -2;
    cap->updateCapacity(I, dt_hour); // qmx = 10, q0 = 5
    model->updateVoltage(cap->q0(), cap->qmax(), cap->I(), 293, dt_hour);
    EXPECT_NEAR(model->cell_voltage(), 3.64, tol);
    EXPECT_NEAR(cap->q0(), 5, 1e-3);

    I = 5;
    cap->updateCapacity(I, dt_hour); // qmx = 10, I = 4.5, q0 = 0.5
    model->updateVoltage(cap->q0(), cap->qmax(), cap->I(), 293, dt_hour);
    EXPECT_NEAR(model->cell_voltage(), 3.71, tol);
    EXPECT_NEAR(cap->q0(), 4.975, 1e-3);
}

TEST_F(voltage_vanadium_lib_battery_voltage_test, calculateMaxChargeHourly){
    double dt_hour = 1;
    CreateModel(dt_hour);

    // start at half SOC
    double max_current;
    double power = model->calculate_max_charge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, -2579, 1);
    double max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 293);
    EXPECT_NEAR(max_current_calc, -4.70, 1e-2);
    // max current reduced to enforce SOC
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 95, 1e-2);


    // start at full SOC
    power = model->calculate_max_charge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, -251, 1);
    max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 293);
    EXPECT_NEAR(max_current_calc, -0.45, 1e-2);
    // max current reduced to enforce SOC
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 95, 1e-2);


    // start at 5 SOC
    double I = 2;
    while (cap->SOC() > 5)
        cap->updateCapacity(I, dt_hour);
    power = model->calculate_max_charge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, -5032, 1);
    max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 293);
    EXPECT_NEAR(max_current_calc, -9.02, 1e-2);
    // max current reduced to enforce SOC
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 95, 1e-2);
}

TEST_F(voltage_vanadium_lib_battery_voltage_test, calculateMaxChargeSubHourly){
    double dt_hour = 0.5;
    CreateModel(dt_hour);

    // start at half SOC
    double max_current;
    double power = model->calculate_max_charge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, -5312, 1);
    double max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 293);
    EXPECT_NEAR(max_current_calc, -9.426, 1e-3);
    // max current reduced to enforce SOC
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 95, 1e-3);


    // start at full SOC
    power = model->calculate_max_charge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, -503, 1);
    max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 293);
    EXPECT_NEAR(max_current_calc, -0.907, 1e-3);
    // max current reduced to enforce SOC
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 95, 1e-3);


    // start at 5 SOC
    double I = 2;
    while (cap->SOC() > 5)
        cap->updateCapacity(I, dt_hour);
    power = model->calculate_max_charge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, -10622, 1);
    max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 293);
    EXPECT_NEAR(max_current_calc, -18.12, 1e-2);
    // max current reduced to enforce SOC
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 95, 1e-3);
}

TEST_F(voltage_vanadium_lib_battery_voltage_test, calculateMaxChargeSubMinute){
    double dt_hour = 1. / 360;
    CreateModel(dt_hour);

    // start at half SOC
    double max_current;
    double power = model->calculate_max_charge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, -10908720, 1);
    double max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 293);
    EXPECT_NEAR(max_current_calc, max_current, 1e-2 * fabs(max_current));
    // max current reduced to enforce SOC
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 95, 1e-3);


    // start at full SOC
    power = model->calculate_max_charge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, -190152, 1);
    max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 293);
    EXPECT_NEAR(max_current_calc, max_current, 0.1 * fabs(max_current));
    // max current reduced to enforce SOC
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 95, 1e-3);


    // start at 5 SOC
    double I = 2;
    while (cap->SOC() > 5)
        cap->updateCapacity(I, dt_hour);
    power = model->calculate_max_charge_w(cap->q0(), cap->qmax(), 0, &max_current);
    EXPECT_NEAR(power, -37840248, 1);
    max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 293);
    EXPECT_NEAR(max_current_calc, max_current, 1e-2 * fabs(max_current));
    // max current reduced to enforce SOC
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 95, 1e-3);
}

TEST_F(voltage_vanadium_lib_battery_voltage_test, calculateMaxDischargeHourly){
    double dt_hour = 1;
    CreateModel(dt_hour);

    // start at half SOC
    double max_current;
    double power = model->calculate_max_discharge_w(cap->q0(), cap->qmax(), 293, &max_current);
    EXPECT_NEAR(power, 2308, 1);        // current ~4
    double max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 293);
    EXPECT_NEAR(max_current_calc, 4.89, 1e-2);
    // Does not empty battery for highest power
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 5, 1e-3);

    // start at empty SOC
    power = model->calculate_max_discharge_w(cap->q0(), cap->qmax(), 293, &max_current);
    EXPECT_NEAR(power, 213, 1);
    max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 293);
    EXPECT_NEAR(max_current_calc, max_current, 1e-1);
    // Empties battery for highest power
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 5, 1e-3);

    // start at full SOC
    double I = -2;
    while (cap->SOC() < 95)
        cap->updateCapacity(I, dt_hour);
    power = model->calculate_max_discharge_w(cap->q0(), cap->qmax(), 293, &max_current);
    EXPECT_NEAR(power, 4570, 1);
    max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 293);
    EXPECT_NEAR(max_current_calc, 9.316, 1e-2);
    // Does not empty battery for highest power
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 5, 1e-3);
}

TEST_F(voltage_vanadium_lib_battery_voltage_test, calculateMaxDischargeSubHourly){
    double dt_hour = 0.5;
    CreateModel(dt_hour);

    // start at half SOC
    double max_current;
    double power = model->calculate_max_discharge_w(cap->q0(), cap->qmax(), 293, &max_current);
    EXPECT_NEAR(power, 4729, 1);        // current ~4
    double max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 293);
    EXPECT_NEAR(max_current_calc, 9.611, 1e-2);
    // Does not empty battery for highest power
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 5, 1e-3);

    // start at empty SOC
    power = model->calculate_max_discharge_w(cap->q0(), cap->qmax(), 293, &max_current);
    EXPECT_NEAR(power, 425, 1);
    max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 293);
    EXPECT_NEAR(max_current_calc, max_current, 1e-1);
    // Empties battery for highest power
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 5, 1e-3);

    // start at full SOC
    double I = -2;
    while (cap->SOC() < 95)
        cap->updateCapacity(I, dt_hour);
    power = model->calculate_max_discharge_w(cap->q0(), cap->qmax(), 293, &max_current);
    EXPECT_NEAR(power, 9617, 1);
    max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 293);
    EXPECT_NEAR(max_current_calc, max_current, 1e-2);
    // Does not empty battery for highest power
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 5, 1e-3);
}

TEST_F(voltage_vanadium_lib_battery_voltage_test, calculateMaxDischargeSubMinute){
    double dt_hour = 1. / 200;
    CreateModel(dt_hour);

    // start at half SOC
    double max_current;
    double power = model->calculate_max_discharge_w(cap->q0(), cap->qmax(), 293, &max_current);
    EXPECT_NEAR(power, 2015656, 1);        // current ~4
    double max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 293);
    EXPECT_NEAR(max_current_calc, 733.51, 1e-2);
    // Does not empty battery for highest power
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 13.324, 1e-3);

    // start at empty SOC
    power = model->calculate_max_discharge_w(cap->q0(), cap->qmax(), 293, &max_current);
    EXPECT_NEAR(power, 71831, 1);
    max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 293);
    EXPECT_NEAR(max_current_calc, max_current, 1e-1);
    // Does not empty battery for highest power
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 8.641, 1e-3);

    // start at full SOC
    double I = -2;
    while (cap->SOC() < 95)
        cap->updateCapacity(I, dt_hour);
    power = model->calculate_max_discharge_w(cap->q0(), cap->qmax(), 293, &max_current);
    EXPECT_NEAR(power, 8903223, 1);
    max_current_calc = model->calculate_current_for_target_w(power, cap->q0(), cap->qmax(), 293);
    EXPECT_NEAR(max_current_calc, 1621.4, 1e-2);
    // Does not empty battery for highest power
    cap->updateCapacity(max_current, dt_hour);
    EXPECT_NEAR(cap->SOC(), 13.93, 1e-3);
}
