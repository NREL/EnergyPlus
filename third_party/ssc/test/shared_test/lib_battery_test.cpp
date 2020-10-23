#include <cmath>
#include <gtest/gtest.h>

#include "logger.h"
#include "lib_battery_test.h"

TEST_F(lib_battery_thermal_test, SetUpTest){
    CreateModel(Cp);
    EXPECT_NEAR(model->T_battery(), 16.85, tol);
    EXPECT_NEAR(model->capacity_percent(), 100, tol);
}

TEST_F(lib_battery_thermal_test, updateTemperatureTest) {
    CreateModel(Cp);
    // battery which adjusts quickly to temp {16.85, 16.85, 21.85, 21.85, 16.85, -3.15, -3.15};
    double I = 50;
    size_t idx = 0;
    model->updateTemperature(I, idx++);
    auto s = thermal_state({93.49, 16.86, 16.85});
    compareState(model->get_state(), s, "updateTemperatureTest: 1");

    I = -50;
    model->updateTemperature(I, idx++);
    s = thermal_state({93.49, 16.87, 16.85, 0.00017});
    compareState(model->get_state(), s, "updateTemperatureTest: 2");

    I = 50;
    model->updateTemperature(I, idx++);
    s = thermal_state({94.02, 17.51, 21.85, -0.17533});
    compareState(model->get_state(), s, "updateTemperatureTest: 3");

    I = 10;
    model->updateTemperature(I, idx++);
    s = thermal_state({94.88, 18.58, 21.85, -0.13172});
    compareState(model->get_state(), s, "updateTemperatureTest: 4");

    I = 10;
    model->updateTemperature(I, idx++);
    s = thermal_state({95.00, 18.76, 16.85, 0.07658});
    compareState(model->get_state(), s, "updateTemperatureTest: 5");

    I = 10;
    model->updateTemperature(I, idx++);
    s = thermal_state({92.55, 15.69, -3.15, 0.75990});
    compareState(model->get_state(), s, "updateTemperatureTest: 6");

    I = 100;
    model->updateTemperature(I, idx++);
    s = thermal_state({88.80, 11.01, -3.15, 0.5714});
    compareState(model->get_state(), s, "updateTemperatureTest: 7");
}

TEST_F(lib_battery_thermal_test, updateTemperatureTestSubMinute) {
    CreateModelSixSecondStep(Cp);
    // battery which adjusts quickly to temp {16.85, 16.85, 21.85, 21.85, 16.85, -3.15, -3.15};
    double I = 50;
    size_t idx = 0;
    double avgTemp = 0;
    for (size_t j = 0; j < 600; j++)
    {
        model->updateTemperature(I, idx);
        avgTemp += model->get_state().T_batt;
    }
    avgTemp /= 600.0;
    EXPECT_NEAR(avgTemp, 16.86, 0.02) << "updateTemperatureTest: 1";
    auto s = thermal_state({ 93.49, 16.86, 16.85 });
    compareState(model->get_state(), s, "updateTemperatureTest: 1");

    I = -50;
    idx++;
    avgTemp = 0;
    for (size_t j = 0; j < 600; j++)
    {
        model->updateTemperature(I, idx);
        avgTemp += model->get_state().T_batt;
    }
    avgTemp /= 600.0;
    EXPECT_NEAR(avgTemp, 16.87, 0.02) << "updateTemperatureTest: 2";
    s = thermal_state({ 93.49, 16.87, 16.85 });
    compareState(model->get_state(), s, "updateTemperatureTest: 2");

    I = 50;
    idx++;
    avgTemp = 0;
    for (size_t j = 0; j < 600; j++)
    {
        model->updateTemperature(I, idx);
        avgTemp += model->get_state().T_batt;
    }
    avgTemp /= 600.0;
    EXPECT_NEAR(avgTemp, 17.51, 0.02) << "updateTemperatureTest: 3";
    s = thermal_state({ 94.47, 18.09, 21.85, -0.1514});
    compareState(model->get_state(), s, "updateTemperatureTest: 3");

    I = 10;
    idx++;
    avgTemp = 0;
    for (size_t j = 0; j < 600; j++)
    {
        model->updateTemperature(I, idx);
        avgTemp += model->get_state().T_batt;
    }
    avgTemp /= 600.0;
    EXPECT_NEAR(avgTemp, 18.59, 0.02) << "updateTemperatureTest: 4";
    s = thermal_state({ 95.22, 19.03, 21.85, -0.1138});
    compareState(model->get_state(), s, "updateTemperatureTest: 4");

    I = 10;
    idx++;
    avgTemp = 0;
    for (size_t j = 0; j < 600; j++)
    {
        model->updateTemperature(I, idx);
        avgTemp += model->get_state().T_batt;
    }
    avgTemp /= 600.0;
    EXPECT_NEAR(avgTemp, 18.74, 0.02) << "updateTemperatureTest: 5";
    s = thermal_state({ 94.79, 18.49, 16.85, 0.06618});
    compareState(model->get_state(), s, "updateTemperatureTest: 5");

    I = 10;
    idx++;
    avgTemp = 0;
    for (size_t j = 0; j < 600; j++)
    {
        model->updateTemperature(I, idx);
        avgTemp += model->get_state().T_batt;
    }
    avgTemp /= 600.0;
    EXPECT_NEAR(avgTemp, 15.69, 0.02) << "updateTemperatureTest: 6";
    s = thermal_state({ 90.49, 13.12, -3.15, 0.6567});
    compareState(model->get_state(), s, "updateTemperatureTest: 6");

    I = 100;
    idx++;
    avgTemp = 0;
    for (size_t j = 0; j < 600; j++)
    {
        model->updateTemperature(I, idx);
        avgTemp += model->get_state().T_batt;
    }
    avgTemp /= 600.0;
    EXPECT_NEAR(avgTemp, 11.01, 0.02) << "updateTemperatureTest: 7";
    s = thermal_state({ 87.27, 9.09, -3.15, 0.4941});
    compareState(model->get_state(), s, "updateTemperatureTest: 7");
}

TEST_F(lib_battery_losses_test, MonthlyLossesTest){
    model = std::unique_ptr<losses_t>(new losses_t(chargingLosses, dischargingLosses, chargingLosses));

    // losses for charging and idling are the same
    int charge_mode = capacity_state::CHARGE;

    size_t idx = 0;
    model->run_losses(idx, dt_hour, charge_mode);
    EXPECT_NEAR(model->getLoss(), 0, tol) << "MonthlyLossesTest: 1";

    idx = 40 * 24;
    model->run_losses(idx, dt_hour, charge_mode);
    EXPECT_NEAR(model->getLoss(), 1, tol) << "MonthlyLossesTest: 2";

    idx = 70 * 24;
    model->run_losses(idx, dt_hour, charge_mode);
    EXPECT_NEAR(model->getLoss(), 2, tol) << "MonthlyLossesTest: 3";

    // discharging
    charge_mode = capacity_state::DISCHARGE;

    idx = 0;
    model->run_losses(idx, dt_hour, charge_mode);
    EXPECT_NEAR(model->getLoss(), 1, tol) << "MonthlyLossesTest: 4";

    idx = 40 * 24;
    model->run_losses(idx, dt_hour, charge_mode);
    EXPECT_NEAR(model->getLoss(), 2, tol) << "MonthlyLossesTest: 5";

    idx = 70 * 24;
    model->run_losses(idx, dt_hour, charge_mode);
    EXPECT_NEAR(model->getLoss(), 3, tol) << "MonthlyLossesTest: 6";

}

TEST_F(lib_battery_losses_test, TimeSeriesLossesTest){
    model = std::unique_ptr<losses_t>(new losses_t(fullLosses));

    int charge_mode = -1;       // not used

    size_t idx = 0;
    model->run_losses(idx, dt_hour, charge_mode);
    EXPECT_NEAR(model->getLoss(), 0, tol) << "TimeSeriesLossesTest: 1";

    idx = 40;
    model->run_losses(idx, dt_hour, charge_mode);
    EXPECT_NEAR(model->getLoss(), 40./8760, tol) << "TimeSeriesLossesTest: 2";

    idx = 70;
    model->run_losses(idx, dt_hour, charge_mode);
    EXPECT_NEAR(model->getLoss(), 70./8760, tol) << "TimeSeriesLossesTest: 3";

}

TEST_F(lib_battery_test, SetUpTest){
    ASSERT_TRUE(1);
}

TEST_F(lib_battery_test, runTestCycleAt1C){
    size_t idx = 0;
    double capacity_passed = 0.;
    double I = Qfull * n_strings;
    batteryModel->run(idx++, I, true);
    capacity_passed += batteryModel->I() * batteryModel->V() / 1000.;
//    std::cerr << "\n" << idx << ": " << capacity_passed << "\n";

    auto s = battery_state_test({{479.75, 1000, 960.01, 20.25, 0, 49.97, 52.09, 2}, // cap
                            550.65, // voltage
                           100, {100, 0, 0, 0, 0, 0, 1, std::vector<double>()}, // cycle
                            {102, 0, 0}, // calendar
                           {96.00, 20.00, 20}, // thermal
                           0});
    compareState(batteryModel, s, "runTestCycleAt1C: 1");

    while (batteryModel->SOC() > SOC_min + 1){
        batteryModel->run(idx++, I, true);
        capacity_passed += batteryModel->I() * batteryModel->V() / 1000.;
    }
//    std::cerr <<  idx << ": soc " << batteryModel->SOC() << ", cap " << capacity_passed << "\n";
    // the SOC isn't at 5 so it means the controller is not able to calculate a current/voltage at which to discharge to 5
    s = battery_state_test({{54.5, 1000, 960.01, 20.25, 0, 5.67, 7.79, 2}, // cap
                       366.96, // voltage
                       100, {100, 0, 0, 0, 0, 0, 1, std::vector<double>()}, // cycle
                        {101.976, 0, 0.0002}, // calendar
                       {96.01, 20.01, 20}, // thermal
                       0});
    compareState(batteryModel, s, "runTestCycleAt1C: 2");

    size_t n_cycles = 400;

    while (n_cycles-- > 0){
        I *= -1;
        while (batteryModel->SOC() < SOC_max - 1){
            batteryModel->run(idx++, I, true);
            capacity_passed += -batteryModel->I() * batteryModel->V() / 1000.;
        }
        I *= -1;
        while (batteryModel->SOC() > SOC_min + 1){
            batteryModel->run(idx++, I, true);
            capacity_passed += batteryModel->I() * batteryModel->V() / 1000.;
        }
    }
//    std::cerr <<  idx << ": soc " << batteryModel->SOC() << ", cap " << capacity_passed << "\n";
    // the SOC isn't at 5 so it means the controller is not able to calculate a current/voltage at which to discharge to 5
    s = battery_state_test({{50.64, 920.75, 883.93, 8.917, 0, 5.73, 6.74, 2}, // cap
                       368.90, // voltage
                       93.08, {92.07, 397, 88.74, 88.72, 88.79, 89.30, 7, std::vector<double>()}, // cycle
                        {98.0, 2739, 0.039}, // calendar
                       {96.0, 20.00, 20}, // thermal
                       32991});
    compareState(batteryModel, s, "runTestCycleAt1C: 3");

    EXPECT_NEAR(capacity_passed, 352736, 1000) << "Current passing through cell";
    double qmax = fmax(s.capacity.qmax_lifetime, s.capacity.qmax_thermal);
    EXPECT_NEAR(qmax/q, .93, 0.01) << "capacity relative to max capacity";
}

TEST_F(lib_battery_test, runTestCycleAt3C){
    size_t idx = 0;
    double capacity_passed = 0.;
    double I = Qfull * n_strings * 3;
    batteryModel->run(idx++, I, true);
    capacity_passed += batteryModel->I() * batteryModel->V() / 1000.;
//    std::cerr << "\n" << idx << ": " << capacity_passed << "\n";

    auto s = battery_state_test({{439.25, 1000, 960.02, 60.75, 0, 45.75, 52.08, 2}, // cap
                            548.35, // voltage
                            100, {100, 0, 0, 0, 0, 0, 1, std::vector<double>()}, // cycle
                             {102, 0}, // calendar
                            {96.01, 20.01, 20}, // thermal
                            0});
    compareState(batteryModel, s, "runTest: 1");

    while (batteryModel->SOC() > SOC_min + 1){
        batteryModel->run(idx++, I, true);
        capacity_passed += batteryModel->I() * batteryModel->V() / 1000.;
    }
//    std::cerr <<  idx << ": soc " << batteryModel->SOC() << ", cap " << capacity_passed << "\n";
    // the SOC isn't at 5 so it means the controller is not able to calculate a current/voltage at which to discharge to 5
    s = battery_state_test({{48.01, 1000, 960.11, 26.74, 0, 5.00, 7.78, 2}, // cap
                       338.91, // voltage
                       101.98, {100, 0, 0, 0, 0, 0, 1, std::vector<double>()}, // cycle
                        {101.98, 0}, // calendar
                       {96.01, 20.01, 20}, // thermal
                       0});
    compareState(batteryModel, s, "runTest: 2");

    size_t n_cycles = 400;

    while (n_cycles-- > 0){
        I *= -1;
        while (batteryModel->SOC() < SOC_max - 1){
            batteryModel->run(idx++, I, true);
            capacity_passed += -batteryModel->I() * batteryModel->V() / 1000.;
        }
        I *= -1;
        while (batteryModel->SOC() > SOC_min + 1){
            batteryModel->run(idx++, I, true);
            capacity_passed += batteryModel->I() * batteryModel->V() / 1000.;
        }
    }
//    std::cerr <<  idx << ": soc " << batteryModel->SOC() << ", cap " << capacity_passed << "\n";
    // the SOC isn't at 5 so it means the controller is not able to calculate a current/voltage at which to discharge to 5
    s = battery_state_test({{49.06, 920.77, 883.94, 8.89, 0, 5.55, 6.55, 2}, // cap
                            362.25, // voltage
                       93.08, {92.08, 397, 88.51, 89.14, 88.53, 89.45, 7, std::vector<double>()}, // cycle
                        {98.11, 2613, 0.0393}, // calendar
                       {96.01, 20, 20}, // thermal
                       32991});
    compareState(batteryModel, s, "runTest: 3");


    EXPECT_NEAR(capacity_passed, 353328, 100) << "Current passing through cell";
    double qmax = fmax(s.capacity.qmax_lifetime, s.capacity.qmax_thermal);
    EXPECT_NEAR(qmax/q, 0.9209, 0.01) << "capacity relative to max capacity";
}

TEST_F(lib_battery_test, runDuplicates) {
    // get initial state
    auto state = batteryModel->get_state();
    auto cap_state = state.capacity;
    auto volt_state = state.voltage;

    // create a duplicate and discharge it
    auto Battery = new battery_t(*batteryModel);
    double I = 10;
    Battery->run(0, I);

    // get state of initial battery
    auto state2 = batteryModel->get_state();
    auto cap_state2 = state2.capacity;
    auto volt_state2 = state2.voltage;

    // get state of duplicate battery
    auto state3 = Battery->get_state();
    auto cap_state3 = state3.capacity;
    auto volt_state3 = state3.voltage;

    EXPECT_FALSE(*cap_state3 == *cap_state2);
    EXPECT_NE(volt_state3->cell_voltage, volt_state2->cell_voltage);
}

TEST_F(lib_battery_test, createFromParams) {
    auto params = std::make_shared<battery_params>(batteryModel->get_params());
    auto bat = battery_t(params);

    double current = 10;
    double P_orig = batteryModel->run(0, current);
    current = 10;
    double P_new = bat.run(0, current);

    EXPECT_EQ(P_orig, P_new);
}

TEST_F(lib_battery_test,logging) {
    logger log(std::cout);
    auto state = batteryModel->get_state();
    auto params = batteryModel->get_params();

    log << *state.capacity << "\n";
    log << *params.capacity << "\n\n";

    log << *state.voltage << "\n";
    log << *params.voltage << "\n\n";

    log << *state.lifetime << "\n";
    log << *params.lifetime << "\n\n";

    log << *state.thermal << "\n";
    log << *params.thermal << "\n\n";

    log << *state.losses << "\n";
    log << *params.losses << "\n\n";

    log << *state.replacement << "\n";
    log << *params.replacement << "\n\n";

    log << state << "\n";
    log << params << "\n";
}

TEST_F(lib_battery_test, RoundtripEffModel){

    batteryModel->changeSOCLimits(0, 100);

    double full_current = 1000;
    double max_current;
    batteryModel->calculate_max_charge_kw(&max_current);

    std::vector<double> eff_vs_current;
    double current = fabs(max_current) * 0.01;
    while (current < fabs(max_current)){
        capacityModel->updateCapacity(full_current, 1);   //discharge to empty

        size_t n_t = 0;
        current *= -1;
        double input_power = 0.;
        while(capacityModel->SOC() < 100 ){
            double input_current = current;
            capacityModel->updateCapacity(input_current, 1);
            voltageModel->updateVoltage(capacityModel->q0(), capacityModel->qmax(), capacityModel->I(), 0, 1);
            input_power += capacityModel->I() * voltageModel->battery_voltage();
            n_t += 1;
        }

        current *= -1;
        double output_power = 0.;
        while(voltageModel->calculate_max_discharge_w(capacityModel->q0(), capacityModel->qmax(), 0, nullptr) > 0 ){
            double output_current = current;
            capacityModel->updateCapacity(output_current, 1);
            voltageModel->updateVoltage(capacityModel->q0(), capacityModel->qmax(), capacityModel->I(), 0, 1);
            output_power += capacityModel->I() * voltageModel->battery_voltage();
            n_t += 1;

        }
        current += fabs(max_current) / 100.;
        eff_vs_current.emplace_back(fabs(output_power/input_power));
    }
    std::vector<double> eff_expected = {0.99, 0.99, 0.98, 0.98, 0.97, 0.97, 0.96, 0.97, 0.95, 0.95, 0.94, 0.95, 0.93, // i = 12
                                        0.91, 0.93, 0.93, 0.92, 0.92, 0.90, 0.93, 0.88, 0.92, 0.90, 0.88, 0.91, 0.89, // i = 25
                                        0.88, 0.90, 0.90, 0.84, 0.87, 0.88, 0.89, 0.89, 0.81, 0.85, 0.85, 0.86, 0.88, // i = 38
                                        0.88, 0.87, 0.78, 0.81, 0.81, 0.83, 0.84, 0.85, 0.86, 0.87, 0.87, 0.85, 0.79, // i = 51
                                        0.73, 0.75, 0.76, 0.77, 0.78, 0.79, 0.80, 0.81, 0.82, 0.83, 0.84, 0.85, 0.85, // i = 64
                                        0.84, 0.84, 0.82, 0.76, 0.65, 0.66, 0.66, 0.67, 0.68, 0.69, 0.70, 0.70, 0.71, // i = 77
                                        0.72, 0.73, 0.74, 0.74, 0.75, 0.76, 0.77, 0.77, 0.78, 0.78, 0.79, 0.80, 0.81, // i = 90
                                        0.81, 0.81, 0.81, 0.81, 0.82, 0.82, 0.81, 0.81, // i = 98
    };
    for (size_t i = 0; i < eff_expected.size(); i++)
        EXPECT_NEAR(eff_vs_current[i], eff_expected[i], .01) << " i = " << i;
}

TEST_F(lib_battery_test, RoundtripEffTable){
    std::vector<double> vals = {0, Vfull, 1.78, Vexp,
                                88.9, Vnom, 99, 0};
    util::matrix_t<double> table(4, 2, &vals);

    auto capacityModel = new capacity_lithium_ion_t(q, SOC_init, SOC_max, SOC_min, dtHour);
    auto voltageModel = new voltage_table_t(n_series, n_strings, Vnom_default, table, resistance, 1);
    capacityModel->change_SOC_limits(0, 100);

    double full_current = 1000;
    double max_current;
    voltageModel->calculate_max_charge_w(capacityModel->q0(), capacityModel->qmax(), 0, &max_current);

    std::vector<double> eff_vs_current;
    double current = fabs(max_current) * 0.01;
    while (current < fabs(max_current)){
        capacityModel->updateCapacity(full_current, 1);   //discharge to empty

        size_t n_t = 0;
        current *= -1;
        double input_power = 0.;
        while(capacityModel->SOC() < 100 ){
            double input_current = current;
            capacityModel->updateCapacity(input_current, 1);
            voltageModel->updateVoltage(capacityModel->q0(), capacityModel->qmax(), capacityModel->I(), 0, 1);
            input_power += capacityModel->I() * voltageModel->battery_voltage();
            n_t += 1;
        }

        current *= -1;
        double output_power = 0.;
        while(voltageModel->calculate_max_discharge_w(capacityModel->q0(), capacityModel->qmax(), 0, nullptr) > 0 ){
            double output_current = current;
            capacityModel->updateCapacity(output_current, 1);
            voltageModel->updateVoltage(capacityModel->q0(), capacityModel->qmax(), capacityModel->I(), 0, 1);
            output_power += capacityModel->I() * voltageModel->battery_voltage();
            n_t += 1;
        }
        current += fabs(max_current) / 100.;
        eff_vs_current.emplace_back(fabs(output_power/input_power));
    }

    std::vector<double> eff_expected = {0.99, 0.99, 0.98, 0.98, 0.97, 0.96, 0.96, 0.95, 0.95, 0.94, 0.93, 0.94, 0.93,
                                        0.92, 0.92, 0.92, 0.91, 0.90, 0.90, 0.89, 0.88, 0.87, 0.88, 0.87, 0.86, 0.87,
                                        0.86, 0.84, 0.86, 0.87, 0.85, 0.83, 0.81, 0.84, 0.85, 0.86, 0.84, 0.82, 0.79,
                                        0.78, 0.80, 0.82, 0.84, 0.85, 0.85, 0.82, 0.80, 0.77, 0.74, 0.73, 0.74, 0.76,
                                        0.77, 0.78, 0.79, 0.81, 0.82, 0.83, 0.84, 0.83, 0.80, 0.77, 0.74, 0.71, 0.67,
                                        0.64, 0.65, 0.65, 0.66, 0.67, 0.68, 0.69, 0.70, 0.71, 0.71, 0.72, 0.73, 0.74,
                                        0.75, 0.75, 0.76, 0.77, 0.78, 0.79, 0.79, 0.80, 0.81, 0.82, 0.82, 0.79, 0.76,
                                        0.73, 0.69, 0.66, 0.62, 0.59, 0.55, 0.51, 0.47};
    for (size_t i = 0; i < eff_expected.size(); i++)
        EXPECT_NEAR(eff_vs_current[i], eff_expected[i], .01);
}

TEST_F(lib_battery_test, RoundtripEffVanadiumFlow){
    auto vol = new voltage_vanadium_redox_t(1, 1, 1.41, 0.001, dtHour);
    auto cap = new capacity_lithium_ion_t(11, 30, 100, 0, dtHour);

    cap->change_SOC_limits(0, 100);


    double full_current = 1000;
    double max_current;
    vol->calculate_max_charge_w(cap->q0(), cap->qmax(), 300, &max_current);

    double current = fabs(max_current) * 0.01;
    while (current < fabs(max_current)){
        cap->updateCapacity(full_current, 1);   //discharge to empty

        std::vector<double> inputs, outputs;

        size_t n_t = 0;
        current *= -1;
        double input_power = 0.;
        while(cap->SOC() < 100 ){
            double input_current = current;
            cap->updateCapacity(input_current, 1);
            vol->updateVoltage(cap->q0(), cap->qmax(), cap->I(), 300, 1);
            input_power += cap->I() * vol->battery_voltage();
            n_t += 1;
            inputs.push_back(vol->battery_voltage());
        }

        current *= -1;
        double output_power = 0.;
        while(vol->calculate_max_discharge_w(cap->q0(), cap->qmax(), 300, nullptr) > 0 ){
            double output_current = current;
            cap->updateCapacity(output_current, 1);
            vol->updateVoltage(cap->q0(), cap->qmax(), cap->I(), 300, 1);
            output_power += cap->I() * vol->battery_voltage();
            n_t += 1;
            outputs.push_back(vol->battery_voltage());
        }

//        std::reverse(outputs.begin(), outputs.end());
//        for (size_t i = 0; i < inputs.size(); i++) {
//            printf("%f, %f\n", inputs[i], outputs[i]);
//        }
//        printf("current %f, eff %f, n %zd\n", current, -output_power/input_power, n_t);

        current += fabs(max_current) / 100.;
    }
}

TEST_F(lib_battery_test, HourlyVsSubHourly)
{
    auto cap_hourly = new capacity_lithium_ion_t(q, SOC_init, SOC_max, SOC_min, dtHour);
    auto volt_hourly = new voltage_dynamic_t(n_series, n_strings, Vnom_default, Vfull, Vexp, Vnom, Qfull, Qexp, Qnom,
                                             C_rate, resistance, 1);

    auto cap_subhourly = new capacity_lithium_ion_t(q, SOC_init, SOC_max, SOC_min, dtHour);
    auto volt_subhourly = new voltage_dynamic_t(n_series, n_strings, Vnom_default, Vfull, Vexp, Vnom, Qfull, Qexp, Qnom,
                                                C_rate, resistance, .5);

    EXPECT_EQ(cap_hourly->q0(), cap_subhourly->q0());
    EXPECT_EQ(volt_hourly->battery_voltage(), volt_subhourly->battery_voltage());

    double discharge_watts = 100.;
    while (cap_hourly->SOC() > 16){
        double I_hourly = volt_hourly->calculate_current_for_target_w(discharge_watts, cap_hourly->q0(), cap_hourly->qmax(), 0);
        cap_hourly->updateCapacity(I_hourly, 1);
        volt_hourly->updateVoltage(cap_hourly->q0(), cap_hourly->qmax(), cap_hourly->I(), 0, 1);
        EXPECT_NEAR(cap_hourly->I() * volt_hourly->battery_voltage(), discharge_watts, 0.1);

        double I_subhourly = volt_subhourly->calculate_current_for_target_w(discharge_watts, cap_subhourly->q0(), cap_subhourly->qmax(), 0);
        cap_subhourly->updateCapacity(I_subhourly, 0.5);
        volt_subhourly->updateVoltage(cap_subhourly->q0(), cap_subhourly->qmax(), cap_subhourly->I(), 0, 0.5);
        EXPECT_NEAR(cap_subhourly->I() * volt_subhourly->battery_voltage(), discharge_watts, 0.1);

    }
}

TEST_F(lib_battery_test, AugmentCapacity) {
    std::vector<int> replacement_schedule = {1, 1, 1};
    std::vector<double> augmentation_percent = {50, 40, 30};
    batteryModel->setupReplacements(replacement_schedule, augmentation_percent);

    // Correct future approach for augmenting batteries, by treating as separate entities
    std::vector<battery_t *> batteries;
    batteries.push_back(new battery_t(*batteryModel));

    batteries.push_back(new battery_t(*batteryModel));
    batteries[1]->setupReplacements(replacement_schedule, augmentation_percent);

    batteries.push_back(new battery_t(*batteryModel));
    batteries[2]->setupReplacements(replacement_schedule, augmentation_percent);

    size_t i = 0;
    double I = 100;
    double mult = 1.0;
    size_t replaceCount = 0;
    for (size_t y = 0; y < replacement_schedule.size(); y++) {
        for (size_t t = 0; t < 8760; t++) {
            mult = fmod(t, 2) == 0 ? 1 : -1;
            double current = mult * I;
            batteries[replaceCount]->runReplacement(y, t, 0);
            batteries[replaceCount]->run(i, current);
        }
        if (replacement_schedule[y] == 1) {
            replaceCount++;
        }
    }

    EXPECT_EQ(batteries[0]->getNumReplacementYear(), 0);
    EXPECT_EQ(batteries[1]->getNumReplacementYear(), 1);
    EXPECT_EQ(batteries[2]->getNumReplacementYear(), 1);

    for (auto i : batteries)
        delete i;
}

TEST_F(lib_battery_test, ReplaceByCapacityTest){
    batteryModel->setupReplacements(91);

    size_t idx = 0;
    double I = Qfull * n_strings * 2;
    while (idx < 100000) {
        batteryModel->run(idx, I);
        batteryModel->runReplacement(0, idx, 0);
        idx ++;
        I = -Qfull * n_strings * 2;
        batteryModel->run(idx, I);
        batteryModel->runReplacement(0, idx, 0);
        idx ++;
    }
    double rep = batteryModel->getNumReplacementYear();
    EXPECT_EQ(rep, 1);
}
