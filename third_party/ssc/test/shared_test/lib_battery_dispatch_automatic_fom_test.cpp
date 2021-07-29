#include "lib_battery_dispatch_automatic_fom_test.h"


TEST_F(AutoFOM_lib_battery_dispatch, DispatchFOMInput) {
    double dtHourFOM = 1.0;
    CreateBattery(dtHourFOM);
    dispatchAuto = new dispatch_automatic_front_of_meter_t(batteryModel, dtHourFOM, 15, 95, 1, 999, 999, max_power, max_power,
                                                           max_power, max_power, 1, dispatch_t::FOM_CUSTOM_DISPATCH, dispatch_t::FRONT, 1, 24, 1, true, true, false, true, 0,
                                                          replacementCost, 0, cyclingCost, ppaRate, ur, 98, 98, 98);

    std::vector<double> P_batt = {-336.062, 336.062};

    batteryPower = dispatchAuto->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;
    batteryPower->powerSystem = 750;
    batteryPower->powerFuelCell = 300;

    dispatchAuto->set_custom_dispatch(P_batt);

    // battery charging from PV
    EXPECT_FALSE(batteryPower->canGridCharge);
    dispatchAuto->update_dispatch(0, 0, 0, 0);
    EXPECT_NEAR(batteryPower->powerBatteryTarget, -322.6, 0.1);
    dispatchAuto->dispatch(0, 0, 0);

    EXPECT_NEAR(batteryPower->powerBatteryDC, -322.6, 0.1);
    EXPECT_NEAR(batteryPower->powerBatteryAC, -336.1, 0.1);
    EXPECT_NEAR(batteryPower->powerGridToBattery, 0, 0.1);
    EXPECT_NEAR(dispatchAuto->battery_model()->SOC(), 50.2, 1e-2);

    dispatchAuto->update_dispatch(0, 0, 0, 1);
    EXPECT_NEAR(batteryPower->powerBatteryTarget, 350.0, 0.1);
    dispatchAuto->dispatch(0, 1, 0);

    EXPECT_NEAR(batteryPower->powerBatteryDC, 350, 0.1);
    EXPECT_NEAR(batteryPower->powerBatteryAC, 336, 0.1);
    EXPECT_NEAR(batteryPower->powerGridToBattery, 0, 0.1);
}

TEST_F(AutoFOM_lib_battery_dispatch, DispatchFOMInputWithLosses) {
    double dtHourFOM = 1.0;
    CreateBatteryWithLosses(dtHourFOM);
    dispatchAuto = new dispatch_automatic_front_of_meter_t(batteryModel, dtHourFOM, 15, 95, 1, 999, 999, max_power, max_power,
        max_power, max_power, 1, dispatch_t::FOM_CUSTOM_DISPATCH, dispatch_t::FRONT, 1, 24, 1, true, true, false, true, 0,
        replacementCost, 0, cyclingCost, ppaRate, ur, 98, 98, 98);

    std::vector<double> P_batt = { -336.062, 336.062 };

    batteryPower = dispatchAuto->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;
    batteryPower->powerSystem = 750;
    batteryPower->powerFuelCell = 300;

    dispatchAuto->set_custom_dispatch(P_batt);

    // battery charging from PV
    EXPECT_FALSE(batteryPower->canGridCharge);
    dispatchAuto->update_dispatch(0, 0, 0, 0);
    EXPECT_NEAR(batteryPower->powerBatteryTarget, -322.6, 0.1);
    dispatchAuto->dispatch(0, 0, 0);

    EXPECT_NEAR(batteryPower->powerBatteryDC, -322.6, 0.1); 
    EXPECT_NEAR(batteryPower->powerBatteryAC, -336.1, 0.1); // Expect charging to remain unchanged, losses will come from the grid
    EXPECT_NEAR(batteryPower->powerGridToBattery, 0, 0.1);
    EXPECT_NEAR(batteryPower->powerSystemLoss, 10.0, 0.1);
    EXPECT_NEAR(dispatchAuto->battery_model()->SOC(), 50.2, 1e-2);

    dispatchAuto->update_dispatch(0, 0, 0, 1);
    EXPECT_NEAR(batteryPower->powerBatteryTarget, 370.9, 0.1);
    dispatchAuto->dispatch(0, 1, 0);

    EXPECT_NEAR(batteryPower->powerBatteryDC, 370.9, 0.1);
    EXPECT_NEAR(batteryPower->powerBatteryAC, 356, 0.1); // Dispatch increases to cover loss
    EXPECT_NEAR(batteryPower->powerGridToBattery, 0, 0.1);
    EXPECT_NEAR(batteryPower->powerSystemLoss, 20.0, 0.1);
}

TEST_F(AutoFOM_lib_battery_dispatch, DispatchFOMInputSubhourly) {
    double dtHourFOM = 0.5;
    CreateBattery(dtHourFOM);
    dispatchAuto = new dispatch_automatic_front_of_meter_t(batteryModel, dtHourFOM, 15, 95, 1, 999, 999, max_power, max_power,
                                                           max_power, max_power, 1, dispatch_t::FOM_CUSTOM_DISPATCH, dispatch_t::FRONT, 1, 24, 1, true, true, false, true, 0,
                                                            replacementCost, 0, cyclingCost, ppaRate, ur, 98, 98, 98);

    std::vector<double> P_batt = {-336.062, 336.062};

    batteryPower = dispatchAuto->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;
    batteryPower->powerSystem = 750;
    batteryPower->powerFuelCell = 300;

    dispatchAuto->set_custom_dispatch(P_batt);

    // battery charging from PV to full maximum_SOC
    EXPECT_FALSE(batteryPower->canGridCharge);
    dispatchAuto->update_dispatch(0, 0, 0, 0);
    EXPECT_NEAR(batteryPower->powerBatteryTarget, -322.6, 0.1);
    dispatchAuto->dispatch(0, 0, 0);

    EXPECT_NEAR(batteryPower->powerBatteryDC, -322.6, 0.1);
    EXPECT_NEAR(batteryPower->powerBatteryAC, -336.1, 0.1);
    EXPECT_NEAR(batteryPower->powerGridToBattery, 0, 0.1);
    EXPECT_NEAR(dispatchAuto->battery_model()->SOC(), 50.1, 0.1);

    dispatchAuto->update_dispatch(0, 0, 0, 1);
    EXPECT_NEAR(batteryPower->powerBatteryTarget, 350.0, 0.1);
    dispatchAuto->dispatch(0, 0, 1);

    EXPECT_NEAR(batteryPower->powerBatteryDC, 350, 0.1);
    EXPECT_NEAR(batteryPower->powerBatteryAC, 336, 0.1);
    EXPECT_NEAR(batteryPower->powerGridToBattery, 0, 0.1);
}

TEST_F(AutoFOM_lib_battery_dispatch, DispatchFOM_DCCustomCharge) {
    double dtHour = 1;
    CreateBattery(dtHour);
    dispatchAuto = new dispatch_automatic_front_of_meter_t(batteryModel, dtHour, 10, 100, 1, 49960, 49960, max_power,
                                                           max_power, max_power, max_power, 1, dispatch_t::FOM_CUSTOM_DISPATCH, dispatch_t::FRONT, 1, 18, 1, true,
                                                           true, true, false, 77000, replacementCost, 1, cyclingCost, ppaRate, ur, 98, 98,
                                                           98);

    std::vector<double> P_batt(6, -25000);

    // battery setup
    dispatchAuto->update_pv_data(pv);
    dispatchAuto->update_cliploss_data(clip);
    dispatchAuto->set_custom_dispatch(P_batt);
    batteryPower = dispatchAuto->getBatteryPower();
    batteryPower->connectionMode = ChargeController::DC_CONNECTED;
    batteryPower->voltageSystem = 600;
    batteryPower->canGridCharge = true;
    batteryPower->setSharedInverter(m_sharedInverter);
    batteryPower->inverterEfficiencyCutoff = 0;

    batteryPower->powerGeneratedBySystem = 0;
    batteryPower->powerSystem = 0;
    batteryPower->powerSystemClipped = 0;

    std::vector<double> SOC = {64.42, 78.77, 93.06, 100., 100., 100.};
    for (size_t h = 0; h < 6; h++) {
        dispatchAuto->update_dispatch(0, 0, h, 0);
        EXPECT_NEAR(batteryPower->powerBatteryTarget, -25000, 0.1) << "error in expected target at hour " << h;

        dispatchAuto->dispatch(0, h, 0);

        EXPECT_NEAR(dispatchAuto->battery_soc(), SOC[h], 0.1);

        if (h < 3){
            EXPECT_NEAR(batteryPower->powerBatteryDC, -25000, 100) << "error in dispatched power at hour " << h;
            EXPECT_NEAR(batteryPower->powerGridToBattery, 25868, 100) << "hour " << h;
            EXPECT_NEAR(batteryPower->sharedInverter->efficiencyAC, 97.6, 0.1);
        }
        else if (h == 3){
            EXPECT_NEAR(batteryPower->powerBatteryDC, -12207, 100) << "error in dispatched power at hour " << h;
            EXPECT_NEAR(batteryPower->powerGridToBattery, 12589, 100) << "hour " << h;
            EXPECT_NEAR(batteryPower->sharedInverter->efficiencyAC, 97.9, 0.1);
        }
        else{
            EXPECT_NEAR(batteryPower->powerBatteryDC, 0, 1e-3) << "error in dispatched power at hour " << h;
            EXPECT_NEAR(batteryPower->powerGridToBattery, 0, 1) << "hour " << h;
            EXPECT_NEAR(batteryPower->sharedInverter->efficiencyAC, 0, 0.1);
        }
    }
}

TEST_F(AutoFOM_lib_battery_dispatch, DispatchFOM_DCCustomChargeSubhourly) {
    double dtHour = 0.5;
    CreateBattery(dtHour);
    dispatchAuto = new dispatch_automatic_front_of_meter_t(batteryModel, dtHour, 10, 100, 1, 49960, 49960, max_power,
                                                           max_power, max_power, max_power, 1, dispatch_t::FOM_CUSTOM_DISPATCH, dispatch_t::FRONT, 1, 18, 1, true,
                                                           true, true, false, 77000, replacementCost, 1, cyclingCost, ppaRate, ur, 98, 98,
                                                           98);

    std::vector<double> P_batt(12, -25000);

    // battery setup
    dispatchAuto->update_pv_data(pv);
    dispatchAuto->update_cliploss_data(clip);
    dispatchAuto->set_custom_dispatch(P_batt);
    batteryPower = dispatchAuto->getBatteryPower();
    batteryPower->connectionMode = ChargeController::DC_CONNECTED;
    batteryPower->voltageSystem = 600;
    batteryPower->canGridCharge = true;
    batteryPower->setSharedInverter(m_sharedInverter);
    batteryPower->inverterEfficiencyCutoff = 0;

    batteryPower->powerGeneratedBySystem = 0;
    batteryPower->powerSystem = 0;
    batteryPower->powerSystemClipped = 0;

    std::vector<double> SOC = {57.24, 64.45, 71.64, 78.81, 85.97, 93.12, 100.00, 100.00, 100.00, 100.00, 100.00, 100.00};
    for (size_t h = 0; h < 12; h++) {
        size_t hour_of_year = hour_of_year_from_index(h, dtHour);
        size_t step = step_from_index(h, dtHour);

        dispatchAuto->update_dispatch(0, hour_of_year, step, h);
        EXPECT_NEAR(batteryPower->powerBatteryTarget, -25000, 0.1) << "error in expected target at hour " << h;

        dispatchAuto->dispatch(0, hour_of_year, step);

        EXPECT_NEAR(dispatchAuto->battery_soc(), SOC[h], 0.1) << "hour " << h;

        if (h < 6){
            EXPECT_NEAR(batteryPower->powerBatteryDC, -25000, 1) << "error in dispatched power at hour " << h;
            EXPECT_NEAR(batteryPower->powerGridToBattery, 25868, 1) << "hour " << h;
            EXPECT_NEAR(batteryPower->sharedInverter->efficiencyAC, 97.6, 0.1);
        }
        else if (h == 6){
            EXPECT_NEAR(batteryPower->powerBatteryDC, -24392, 1) << "error in dispatched power at hour " << h;
            EXPECT_NEAR(batteryPower->powerGridToBattery, 25233, 1) << "hour " << h;
            EXPECT_NEAR(batteryPower->sharedInverter->efficiencyAC, 97.64, 0.1);
        }
        else{
            EXPECT_NEAR(batteryPower->powerBatteryDC, 0, 1e-3) << "error in dispatched power at hour " << h;
            EXPECT_NEAR(batteryPower->powerGridToBattery, 0, 1) << "hour " << h;
            EXPECT_NEAR(batteryPower->sharedInverter->efficiencyAC, 0, 0.1);
        }
    }
}

TEST_F(AutoFOM_lib_battery_dispatch, DispatchFOM_DCAuto) {
    double dtHour = 1;
    CreateBattery(dtHour);
    dispatchAuto = new dispatch_automatic_front_of_meter_t(batteryModel, dtHour, 10, 100, 1, 49960, 49960, max_power,
                                                           max_power, max_power, max_power, 1, dispatch_t::FOM_LOOK_AHEAD, dispatch_t::FRONT, 1, 18, 1, true, true, false,
                                                           false, 77000, replacementCost, 1, cyclingCost, ppaRate, ur, 98, 98, 98);

    // battery setup
    dispatchAuto->update_pv_data(pv);
    dispatchAuto->update_cliploss_data(clip);
    batteryPower = dispatchAuto->getBatteryPower();
    batteryPower->connectionMode = ChargeController::DC_CONNECTED;
    batteryPower->voltageSystem = 600;
    batteryPower->setSharedInverter(m_sharedInverter);

    std::vector<double> targetkW = {-9767.18, -10052.40, -9202.19, -7205.42, -1854.60, 0., // 0 - 5
                                        -41690.48, -16690.50, -2334.45, -324.19, 0., 0., // 6 - 11
                                        0., 0., 0., 0., 0., 77000, // 12 - 17
                                        77000, 77000, 77000, 77000, 77000, 0. }; // 18 - 23
    std::vector<double> dispatchedkW = { -9767.18, -10052.40, -9202.19, -7205.42, -1854.60, 0., // 0 - 5
                                        -29200.17, -16690.50, -2334.45, -324.19, 0., 0., // 6 - 11
                                        0., 0., 0., 0., 0., 28116.05, // 18
                                        27946.64, 27664.23, 27099.14, 25401.83, 9343.36, 0. }; // 24
    std::vector<double> SOC = {55.72, 61.58, 66.93, 71.12, 72.21, 72.21, // 6
                                88.87, 98.44, 99.78, 99.97, 99.97, 99.97, // 12
                                99.97, 99.97, 99.97, 99.97, 99.97, 83.30, // 12 - 17
                                66.63, 49.97, 33.30, 16.63, 10, 10 }; // 18 - 23
    for (size_t h = 0; h < 24; h++) {
        batteryPower->powerGeneratedBySystem = pv[h];
        batteryPower->powerSystem = pv[h];
        batteryPower->powerSystemClipped = clip[h];

        dispatchAuto->update_dispatch(0, h, 0, h);
        EXPECT_NEAR(batteryPower->powerBatteryTarget, targetkW[h], 0.1) << "error in expected target at hour " << h;

        dispatchAuto->dispatch(0, h, 0);
        EXPECT_NEAR(batteryPower->powerBatteryDC, dispatchedkW[h], 0.1) << "error in dispatched power at hour " << h;
        EXPECT_NEAR(dispatchAuto->battery_soc(), SOC[h], 0.1) << "error in SOC at hour " << h;

    }
}

TEST_F(AutoFOM_lib_battery_dispatch, DispatchFOM_DCAutoWithLosses) {
    double dtHour = 1;
    CreateBatteryWithLosses(dtHour);
    dispatchAuto = new dispatch_automatic_front_of_meter_t(batteryModel, dtHour, 10, 100, 1, 49960, 49960, max_power,
        max_power, max_power, max_power, 1, dispatch_t::FOM_LOOK_AHEAD, dispatch_t::FRONT, 1, 18, 1, true, true, false,
        false, 77000, replacementCost, 1, cyclingCost, ppaRate, ur, 98, 98, 98);

    // battery setup
    dispatchAuto->update_pv_data(pv);
    dispatchAuto->update_cliploss_data(clip);
    batteryPower = dispatchAuto->getBatteryPower();
    batteryPower->connectionMode = ChargeController::DC_CONNECTED;
    batteryPower->voltageSystem = 600;
    batteryPower->setSharedInverter(m_sharedInverter);

    std::vector<double> targetkW = { -9767.18, -10052.40, -9202.19, -7205.42, -1854.60, 0., // 0 - 5
                                    -41690.48, -16690.50, -2334.45, -324.19, 0., 0., // 6 - 11
                                    0., 0., 0., 0., 0., 77005, // 12 - 17
                                    77005, 77005, 77005, 77005, 77005, 0. }; // 18 - 23
    std::vector<double> dispatchedkW = { -9767.18, -10052.40, -9202.19, -7205.42, -1854.60, 0., // 0 - 5
                                        -29200.17, -16690.50, -2334.45, -324.19, 0., 0., // 6 - 11
                                        0., 0., 0., 0., 0., 28116.05, // 18
                                        27946.64, 27664.23, 27099.14, 25401.83, 9343.36, 0. }; // 24
    std::vector<double> SOC = { 55.72, 61.58, 66.93, 71.12, 72.21, 72.21, // 6
                                88.87, 98.44, 99.78, 99.97, 99.97, 99.97, // 12
                                99.97, 99.97, 99.97, 99.97, 99.97, 83.30, // 12 - 17
                                66.63, 49.97, 33.30, 16.63, 10, 10 }; // 18 - 23
    for (size_t h = 0; h < 24; h++) {
        batteryPower->powerGeneratedBySystem = pv[h];
        batteryPower->powerSystem = pv[h];
        batteryPower->powerSystemClipped = clip[h];

        dispatchAuto->update_dispatch(0, h, 0, h);
        EXPECT_NEAR(batteryPower->powerBatteryTarget, targetkW[h], 0.1) << "error in expected target at hour " << h;

        dispatchAuto->dispatch(0, h, 0);
        EXPECT_NEAR(batteryPower->powerBatteryDC, dispatchedkW[h], 0.1) << "error in dispatched power at hour " << h;
        EXPECT_NEAR(dispatchAuto->battery_soc(), SOC[h], 0.1) << "error in SOC at hour " << h;

    }
}
TEST_F(AutoFOM_lib_battery_dispatch, DispatchFOM_DCAutoSubhourly) {
    double dtHour = 0.5;
    CreateBattery(dtHour);
    dispatchAuto = new dispatch_automatic_front_of_meter_t(batteryModel, dtHour, 10, 100, 1, 49960, 49960, max_power,
                                                           max_power, max_power, max_power, 1, dispatch_t::FOM_LOOK_AHEAD, dispatch_t::FRONT, 1, 18, 1, true, true, false,
                                                           false, 77000, replacementCost, 1, cyclingCost, ppaRate, ur, 98, 98, 98);

    // battery setup
    dispatchAuto->update_pv_data(pv);
    dispatchAuto->update_cliploss_data(clip);
    batteryPower = dispatchAuto->getBatteryPower();
    batteryPower->connectionMode = ChargeController::DC_CONNECTED;
    batteryPower->voltageSystem = 600;
    batteryPower->setSharedInverter(m_sharedInverter);

    std::vector<double> targetkW = {-9767.18, -10052.40, -9202.19, -7205.42, -1854.60, 0.};
    std::vector<double> SOC = {52.86, 55.80, 58.49, 60.60, 61.14, 61.14};
    for (size_t h = 0; h < 6; h++) {
        size_t hour_of_year = hour_of_year_from_index(h, dtHour);
        size_t step = step_from_index(h, dtHour);

        batteryPower->powerGeneratedBySystem = pv[h];
        batteryPower->powerSystem = pv[h];
        batteryPower->powerSystemClipped = clip[h];

        dispatchAuto->update_dispatch(0, hour_of_year, step, h);
        EXPECT_NEAR(batteryPower->powerBatteryTarget, targetkW[h], 0.1) << "error in expected target at hour " << h;

        dispatchAuto->dispatch(0, hour_of_year, step);
        EXPECT_NEAR(batteryPower->powerBatteryDC, targetkW[h], 0.1) << "error in dispatched power at hour " << h;
        EXPECT_NEAR(dispatchAuto->battery_soc(), SOC[h], 0.1) << "error in SOC at hour " << h;
    }
}


TEST_F(AutoFOM_lib_battery_dispatch, DispatchFOM_ACCustomCharge) {
    double dtHour = 1;
    CreateBattery(dtHour);
    dispatchAuto = new dispatch_automatic_front_of_meter_t(batteryModel, dtHour, 10, 100, 1, 49960, 49960, max_power,
                                                           max_power, max_power, max_power, 1, dispatch_t::FOM_CUSTOM_DISPATCH, dispatch_t::FRONT, 1, 18, 1, true,
                                                             true, true, false, 77000, replacementCost, 1, cyclingCost, ppaRate, ur, 98, 98,
                                                             98);

    std::vector<double> P_batt(6, -25000);

    // battery setup
    dispatchAuto->update_pv_data(pv);
    dispatchAuto->update_cliploss_data(clip);
    dispatchAuto->set_custom_dispatch(P_batt);
    batteryPower = dispatchAuto->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;
    batteryPower->voltageSystem = 600;
    batteryPower->canGridCharge = true;
    batteryPower->setSharedInverter(m_sharedInverter);

    batteryPower->powerGeneratedBySystem = 0;
    batteryPower->powerSystem = 0;
    batteryPower->powerSystemClipped = 0;

    std::vector<double> SOC = {63.86, 77.64, 91.37, 100.00, 100.00, 100.00};
    for (size_t h = 0; h < 6; h++) {
        dispatchAuto->update_dispatch(0, 0, h, 0);
        EXPECT_NEAR(batteryPower->powerBatteryTarget, -24000, 0.1) << "error in expected target at hour " << h;

        dispatchAuto->dispatch(0, h, 0);

        EXPECT_NEAR(dispatchAuto->battery_soc(), SOC[h],0.1);

        if (h < 3){
            EXPECT_NEAR(batteryPower->powerBatteryDC, -24000, 1) << "error in dispatched power at hour " << h;
            EXPECT_NEAR(batteryPower->powerGridToBattery, 25000, 1) << "hour " << h;
            EXPECT_NEAR(batteryPower->sharedInverter->efficiencyAC, 96, 0.1);
        }
        else if (h == 3){
            EXPECT_NEAR(batteryPower->powerBatteryDC, -15195, 1) << "error in dispatched power at hour " << h;
            EXPECT_NEAR(batteryPower->powerGridToBattery, 15828., 1) << "hour " << h;
            EXPECT_NEAR(batteryPower->sharedInverter->efficiencyAC, 96, 0.1);
        }
        else{
            EXPECT_NEAR(batteryPower->powerBatteryDC, 0, 1e-3) << "error in dispatched power at hour " << h;
            EXPECT_NEAR(batteryPower->powerGridToBattery, 0, 1) << "hour " << h;
            EXPECT_NEAR(batteryPower->sharedInverter->efficiencyAC, 96, 0.1);
        }
    }
}

TEST_F(AutoFOM_lib_battery_dispatch, DispatchFOM_ACCustomChargeSubhourly) {
    double dtHour = 0.5;
    CreateBattery(dtHour);
    dispatchAuto = new dispatch_automatic_front_of_meter_t(batteryModel, dtHour, 10, 100, 1, 49960, 49960, max_power,
                                                           max_power, max_power, max_power, 1, dispatch_t::FOM_CUSTOM_DISPATCH, dispatch_t::FRONT, 1, 18, 1, true,
                                                           true, true, false, 77000, replacementCost, 1, cyclingCost, ppaRate, ur, 98, 98,
                                                           98);

    std::vector<double> P_batt(12, -25000);

    // battery setup
    dispatchAuto->update_pv_data(pv);
    dispatchAuto->update_cliploss_data(clip);
    dispatchAuto->set_custom_dispatch(P_batt);
    batteryPower = dispatchAuto->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;
    batteryPower->voltageSystem = 600;
    batteryPower->canGridCharge = true;
    batteryPower->setSharedInverter(m_sharedInverter);

    batteryPower->powerGeneratedBySystem = 0;
    batteryPower->powerSystem = 0;
    batteryPower->powerSystemClipped = 0;

    std::vector<double> SOC = {56.95, 63.88, 70.79, 77.68, 84.56, 91.43, 98.28, 100.00, 100.00, 100.00, 100.00, 100.00};
    for (size_t h = 0; h < 12; h++) {
        size_t hour_of_year = hour_of_year_from_index(h, dtHour);
        size_t step = step_from_index(h, dtHour);

        dispatchAuto->update_dispatch(0, hour_of_year, step, h);
        EXPECT_NEAR(batteryPower->powerBatteryTarget, -24000, 0.1) << "error in expected target at hour " << h;

        dispatchAuto->dispatch(0, hour_of_year, step);

        EXPECT_NEAR(dispatchAuto->battery_soc(), SOC[h], 0.1);

        if (h < 7){
            EXPECT_NEAR(batteryPower->powerBatteryDC, -24000, 1) << "error in dispatched power at hour " << h;
            EXPECT_NEAR(batteryPower->powerGridToBattery, 25000, 1) << "hour " << h;
            EXPECT_NEAR(batteryPower->sharedInverter->efficiencyAC, 96, 0.1);
        }
        else if (h == 7){
            EXPECT_NEAR(batteryPower->powerBatteryDC, -6022, 1) << "error in dispatched power at hour " << h;
            EXPECT_NEAR(batteryPower->powerGridToBattery, 6273, 1) << "hour " << h;
            EXPECT_NEAR(batteryPower->sharedInverter->efficiencyAC, 96, 0.1);
        }
        else{
            EXPECT_NEAR(batteryPower->powerBatteryDC, 0, 1e-3) << "error in dispatched power at hour " << h;
            EXPECT_NEAR(batteryPower->powerGridToBattery, 0, 1) << "hour " << h;
            EXPECT_NEAR(batteryPower->sharedInverter->efficiencyAC, 96, 0.1);
        }
    }
}

TEST_F(AutoFOM_lib_battery_dispatch, DispatchFOM_ACAuto) {
    double dtHour = 1;
    CreateBattery(dtHour);
    dispatchAuto = new dispatch_automatic_front_of_meter_t(batteryModel, dtHour, 10, 100, 1, 49960, 49960, max_power,
                                                           max_power, max_power, max_power, 1, dispatch_t::FOM_LOOK_AHEAD, dispatch_t::FRONT, 1, 18, 1, true, true, false,
                                                           false, 77000, replacementCost, 1, cyclingCost, ppaRate, ur, 98, 98, 98);

    // battery setup
    dispatchAuto->update_pv_data(pv); // PV Resource is available for the 1st 10 hrs
    dispatchAuto->update_cliploss_data(clip); // Clip charging is available for the 1st 5 hrs
    batteryPower = dispatchAuto->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;
    batteryPower->voltageSystem = 600;
    batteryPower->setSharedInverter(m_sharedInverter);

    std::vector<double> targetkW = {-9767.18, -10052.40, -9202.19, -7205.42, -1854.60, 0.,
                                    -41690.48, -16690.50, -2334.45, -324.19, 0., 0.,
                                    0., 0., 0., 0., 0., 77000,
                                    77000, 77000, 77000, 77000, 77000, 0. };
    std::vector<double> dispatchedkW = { -9767.18, -10052.40, -9202.19, -7205.42, -1854.60, 0.,
                                -29200.17, -16690.50, -2334.45, -324.19, 0., 0.,
                                0., 0., 0., 0., 0., 28116.05,
                                27946.64, 27664.23, 27099.14, 25401.83, 9343.36, 0. };
    std::vector<double> SOC = {55.72, 61.58, 66.93, 71.12, 72.21, 72.21,
                                88.87, 98.44, 99.78, 99.97, 99.97, 99.97,
                                99.97, 99.97, 99.97, 99.97, 99.97, 83.30,
                                66.63, 49.97, 33.30, 16.63, 10.0, 10.0 };
    for (size_t h = 0; h < 24; h++) {
        batteryPower->powerGeneratedBySystem = pv[h];
        batteryPower->powerSystem = pv[h];
        batteryPower->powerSystemClipped = clip[h];

        dispatchAuto->update_dispatch(0, h, 0, h);
        EXPECT_NEAR(batteryPower->powerBatteryTarget, targetkW[h], 0.1) << "error in expected target at hour " << h;

        dispatchAuto->dispatch(0, h, 0);

        EXPECT_NEAR(batteryPower->powerBatteryDC, dispatchedkW[h], 0.1) << "error in dispatched power at hour " << h;
        EXPECT_NEAR(dispatchAuto->battery_soc(), SOC[h], 0.1) << "error in SOC at hour " << h;
    }
}

TEST_F(AutoFOM_lib_battery_dispatch, DispatchFOM_ACAutoWithLosses) {
    double dtHour = 1;
    CreateBatteryWithLosses(dtHour);
    dispatchAuto = new dispatch_automatic_front_of_meter_t(batteryModel, dtHour, 10, 100, 1, 49960, 49960, max_power,
        max_power, max_power, max_power, 1, dispatch_t::FOM_LOOK_AHEAD, dispatch_t::FRONT, 1, 18, 1, true, true, false,
        false, 77000, replacementCost, 1, cyclingCost, ppaRate, ur, 98, 98, 98);

    // battery setup
    dispatchAuto->update_pv_data(pv); // PV Resource is available for the 1st 10 hrs
    dispatchAuto->update_cliploss_data(clip); // Clip charging is available for the 1st 5 hrs
    batteryPower = dispatchAuto->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;
    batteryPower->voltageSystem = 600;
    batteryPower->setSharedInverter(m_sharedInverter);

    std::vector<double> targetkW = { -9767.18, -10052.40, -9202.19, -7205.42, -1854.60, 0.,
                                    -41690.48, -16690.50, -2334.45, -324.19, 0., 0.,
                                    0., 0., 0., 0., 0., 77000,
                                    77000, 77000, 77000, 77000, 77000, 0. };
    std::vector<double> dispatchedkW = { -9767.18, -10052.40, -9202.19, -7205.42, -1854.60, 0.,
                                -29200.17, -16690.50, -2334.45, -324.19, 0., 0.,
                                0., 0., 0., 0., 0., 28116.05,
                                27946.64, 27664.23, 27099.14, 25401.83, 9343.36, 0. }; // Battery was already discharging at max power, it stays unchanged
    std::vector<double> SOC = { 55.72, 61.58, 66.93, 71.12, 72.21, 72.21,
                                88.87, 98.44, 99.78, 99.97, 99.97, 99.97,
                                99.97, 99.97, 99.97, 99.97, 99.97, 83.30,
                                66.63, 49.97, 33.30, 16.63, 10.0, 10.0 };
    for (size_t h = 0; h < 24; h++) {
        batteryPower->powerGeneratedBySystem = pv[h];
        batteryPower->powerSystem = pv[h];
        batteryPower->powerSystemClipped = clip[h];

        dispatchAuto->update_dispatch(0, h, 0, h);
        EXPECT_NEAR(batteryPower->powerBatteryTarget, targetkW[h], 0.1) << "error in expected target at hour " << h;

        dispatchAuto->dispatch(0, h, 0);

        EXPECT_NEAR(batteryPower->powerBatteryDC, dispatchedkW[h], 0.1) << "error in dispatched power at hour " << h;
        EXPECT_NEAR(dispatchAuto->battery_soc(), SOC[h], 0.1) << "error in SOC at hour " << h;
    }
}

TEST_F(AutoFOM_lib_battery_dispatch, DispatchFOM_ACAutoSubhourly) {
    double dtHour = 0.5;
    CreateBattery(dtHour);
    dispatchAuto = new dispatch_automatic_front_of_meter_t(batteryModel, dtHour, 10, 100, 1, 49960, 49960, max_power,
                                                           max_power, max_power, max_power, 1, dispatch_t::FOM_LOOK_AHEAD, dispatch_t::FRONT, 1, 18, 1, true, true, false,
                                                           false, 77000, replacementCost, 1, cyclingCost, ppaRate, ur, 98, 98, 98);

    // battery setup
    dispatchAuto->update_pv_data(pv);
    dispatchAuto->update_cliploss_data(clip);
    batteryPower = dispatchAuto->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;
    batteryPower->voltageSystem = 600;
    batteryPower->setSharedInverter(m_sharedInverter);

    std::vector<double> targetkW = {-9767.18, -10052.40, -9202.19, -7205.42, -1854.60, 0.};
    std::vector<double> SOC = {52.86, 55.80, 58.49, 60.60, 61.14, 61.14};
    for (size_t h = 0; h < 6; h++) {
        size_t hour_of_year = hour_of_year_from_index(h, dtHour);
        size_t step = step_from_index(h, dtHour);

        batteryPower->powerGeneratedBySystem = pv[h];
        batteryPower->powerSystem = pv[h];
        batteryPower->powerSystemClipped = clip[h];

        dispatchAuto->update_dispatch(0, hour_of_year, step, h);
        EXPECT_NEAR(batteryPower->powerBatteryTarget, targetkW[h], 0.1) << "error in expected target at hour " << h;

        dispatchAuto->dispatch(0, hour_of_year, step);
        EXPECT_NEAR(batteryPower->powerBatteryDC, targetkW[h], 0.1) << "error in dispatched power at hour " << h;
        EXPECT_NEAR(dispatchAuto->battery_soc(), SOC[h], 0.1) << "error in SOC at hour " << h;

    }
}

