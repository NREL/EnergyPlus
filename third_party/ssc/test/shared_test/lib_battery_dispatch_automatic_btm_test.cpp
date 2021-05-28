#include "lib_battery_dispatch_automatic_btm_test.h"
#include "code_generator_utilities.h"

#include "../input_cases/shared_rate_data.h"

TEST_F(AutoBTMTest_lib_battery_dispatch, DispatchAutoBTMGridCharging) {
    double dtHour = 1;
    CreateBattery(dtHour);

    dispatchAutoBTM = new dispatch_automatic_behind_the_meter_t(batteryModel, dtHour, SOC_min, SOC_max, currentChoice,
                                                                max_current,
                                                                max_current, max_power, max_power, max_power, max_power,
                                                                0, dispatch_t::BTM_MODES::LOOK_AHEAD, 0, 1, 24, 1, true,
                                                                true, false, false, util_rate, replacementCost, cyclingChoice, cyclingCost);

    // Setup pv and load signal for peak shaving algorithm
    for (size_t h = 0; h < 24; h++) {
        pv_prediction.push_back(0); // Set detailed PV later
        if (h < 4) {
            load_prediction.push_back(600);
        }
        else {
            load_prediction.push_back(0);
        }
    }

    // Set detailed PV
    pv_prediction[0] = 500;
    pv_prediction[1] = 400;
    pv_prediction[2] = 300;
    pv_prediction[3] = 200;
    dispatchAutoBTM->update_load_data(load_prediction);
    dispatchAutoBTM->update_pv_data(pv_prediction);

    batteryPower = dispatchAutoBTM->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;

    EXPECT_EQ(batteryPower->powerBatteryChargeMaxAC, 50);

    // TEST 1: Verify no grid charging since disallowed  (_P_battery_use target is ~ -50)
    dispatchAutoBTM->update_dispatch(0, 0, 0, 0);

    dispatchAutoBTM->dispatch(0, 0, 0);     // original target for battery power is
    EXPECT_EQ(batteryPower->powerGridToBattery, 0);
    EXPECT_NEAR(batteryPower->powerBatteryDC, 0, 0.02);

    // TEST 2: Now, allow grid charging, should charge up to Max Charge Power (enforced by restrict_power)
    batteryPower->canGridCharge = true;
    dispatchAutoBTM->update_dispatch(0, 0, 0, 0);
    dispatchAutoBTM->dispatch(0, 0, 0);
    EXPECT_NEAR(batteryPower->powerGridToBattery, 50, 1);
    EXPECT_NEAR(batteryPower->powerBatteryDC, -48, 1);
}

TEST_F(AutoBTMTest_lib_battery_dispatch, DispatchAutoBTMPVCharging) {
    double dtHour = 1;
    CreateBattery(dtHour);

    dispatchAutoBTM = new dispatch_automatic_behind_the_meter_t(batteryModel, dtHour, SOC_min, SOC_max, currentChoice,
                                                                max_current,
                                                                max_current, max_power, max_power, max_power, max_power,
                                                                0, dispatch_t::BTM_MODES::LOOK_AHEAD, 0, 1, 24, 1, true,
                                                                true, false, false, util_rate, replacementCost, cyclingChoice, cyclingCost);

    // Setup pv and load signal for peak shaving algorithm
    for (size_t h = 0; h < 24; h++) {
        if (h > 6 && h < 18) {
            pv_prediction.push_back(700);
        }
        else {
            pv_prediction.push_back(0);
        }
        load_prediction.push_back(500);
    }

    dispatchAutoBTM->update_load_data(load_prediction);
    dispatchAutoBTM->update_pv_data(pv_prediction);

    batteryPower = dispatchAutoBTM->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;

    // Load never peaks above average load, so battery never discharges
    std::vector<double> expectedPower = {0, 0, 0, 0, 0, 0, 0, -50, -50, -50, -50, -50, -1.94, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                         0, 0, 0, 0, 0};
    for (size_t h = 0; h < 24; h++) {
        if (h > 6 && h < 18) {
            batteryPower->powerSystem = 700; // Match the predicted PV
        }
        else {
            batteryPower->powerSystem = 0;
        }
        batteryPower->powerLoad = 500; // Match the predicted load
        dispatchAutoBTM->dispatch(0, h, 0);
        EXPECT_NEAR(batteryPower->powerBatteryDC, expectedPower[h], 0.2) << " error in expected at hour " << h;
    }
}

TEST_F(AutoBTMTest_lib_battery_dispatch, DispatchAutoBTMPVChargeAndDischarge) {
    double dtHour = 1;
    CreateBattery(dtHour);

    dispatchAutoBTM = new dispatch_automatic_behind_the_meter_t(batteryModel, dtHour, SOC_min, SOC_max, currentChoice,
                                                                max_current,
                                                                max_current, max_power, max_power, max_power, max_power,
                                                                0, dispatch_t::BTM_MODES::LOOK_AHEAD, 0, 1, 24, 1, true,
                                                                true, false, false, util_rate, replacementCost, cyclingChoice, cyclingCost);

    // Setup pv and load signal for peak shaving algorithm
    for (size_t h = 0; h < 24; h++) {
        if (h > 6 && h < 18) {
            pv_prediction.push_back(700);
        }
        else {
            pv_prediction.push_back(0);
        }

        if (h > 18) {
            load_prediction.push_back(600);
        }
        else {
            load_prediction.push_back(500);
        }
    }

    dispatchAutoBTM->update_load_data(load_prediction);
    dispatchAutoBTM->update_pv_data(pv_prediction);

    batteryPower = dispatchAutoBTM->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;

    // Battery will charge when PV is available, then discharge when load increases at 7 pm
    std::vector<double> expectedPower = {0, 0, 0, 0, 0, 0, 0, -50, -50, -50, -50, -50, -1.63, 0, 0, 0, 0, 0, 0, 50, 50,
                                         50, 50, 50, 50, 50, 50};
    for (size_t h = 0; h < 24; h++) {
        batteryPower->powerLoad = 500;
        batteryPower->powerSystem = 0;
        if (h > 6 && h < 18) {
            batteryPower->powerSystem = 700; // Match the predicted PV
        }
        else if (h > 18) {
            batteryPower->powerLoad = 600; // Match the predicted load
        }
        dispatchAutoBTM->dispatch(0, h, 0);
        EXPECT_NEAR(batteryPower->powerBatteryDC, expectedPower[h], 0.5) << " error in expected at hour " << h;
    }
}

TEST_F(AutoBTMTest_lib_battery_dispatch, DispatchAutoBTMPVChargeAndDischargeSubhourly) {
    double dtHour = 0.25;
    CreateBattery(dtHour);

    dispatchAutoBTM = new dispatch_automatic_behind_the_meter_t(batteryModel, dtHour, SOC_min, SOC_max, currentChoice,
                                                                max_current,
                                                                max_current, max_power, max_power, max_power, max_power,
                                                                0, dispatch_t::BTM_MODES::LOOK_AHEAD, 0, 1, 24, 1, true,
                                                                true, false, false, util_rate, replacementCost, cyclingChoice, cyclingCost);

    // Setup pv and load signal for peak shaving algorithm
    for (size_t h = 0; h < 24; h++) {
        for (size_t step = 0; step < 4; step++) {
            if (h > 6 && h < 18) {
                pv_prediction.push_back(700);
            }
            else {
                pv_prediction.push_back(0);
            }

            if (h > 18) {
                load_prediction.push_back(600);
            }
            else {
                load_prediction.push_back(500);
            }
        }
    }

    std::vector<double> expectedPower = {0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
                                         0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
                                         0.00, 0.00, -50.00, -50.00, -50.00, -50.00, -50.00, -50.00, -50.00, -50.00,
                                         -50.00, -50.00, -50.00, -50.00, -50.00, -50.00, -50.00, -50.00, -50.00, -50.00,
                                         -50.00, -50.00, -6.40, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
                                         0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
                                         0.00, 0.00, 0.00, 0.00, 0.00, 50.00, 50.00, 50.00, 50.00, 50.00, 50.00, 50.00,
                                         50.00, 50.00, 50.00, 50.00, 50.00, 50.00, 50.00, 50.00, 50.00, 50.00, 50.00,
                                         50.00, 50.00, 50.00, 50.00, 50.00, 50.00, 50.00, 50.00, 50.00, 50.00, 50.00,
                                         50.00, 50.00, 50.00, 50.00, 50.00, 50.00, 50.00, 50.00, 50.00, 50.00, 50.00,
                                         50.00, 50.00, 50.00, 50.00, 50.00, 50.00, 50.00, 50.00};

    dispatchAutoBTM->update_load_data(load_prediction);
    dispatchAutoBTM->update_pv_data(pv_prediction);

    batteryPower = dispatchAutoBTM->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;

    // Battery will charge when PV is available, then discharge when load increases at 7 pm
    int index = 0;
    for (size_t h = 0; h < 24; h++) {
        for (size_t step = 0; step < 4; step++) {
            batteryPower->powerLoad = 500;
            batteryPower->powerSystem = 0;
            if (h > 6 && h < 18) {
                batteryPower->powerSystem = 700; // Match the predicted PV
            }
            else if (h > 18) {
                batteryPower->powerLoad = 600; // Match the predicted load
            }
            dispatchAutoBTM->dispatch(0, h, step);
            EXPECT_NEAR(batteryPower->powerBatteryDC, expectedPower[index], 0.2) << " error in expected at step " << index;
            index++;
        }
    }
}

TEST_F(AutoBTMTest_lib_battery_dispatch, DispatchAutoBTMDCClipCharge) {
    double dtHour = 1;
    CreateBattery(dtHour);

    // Only charge from clipped power
    dispatchAutoBTM = new dispatch_automatic_behind_the_meter_t(batteryModel, dtHour, SOC_min, SOC_max, currentChoice,
                                                                max_current,
                                                                max_current, max_power, max_power, max_power, max_power,
                                                                0, dispatch_t::BTM_MODES::LOOK_AHEAD, 0, 1, 24, 1,
                                                                false, true, false, false, util_rate, replacementCost, cyclingChoice, cyclingCost);

    // Setup pv and load signal for peak shaving algorithm
    for (size_t h = 0; h < 24; h++) {
        if (h > 6 && h < 18) {
            pv_prediction.push_back(700);
        }
        else {
            pv_prediction.push_back(0);
        }

        if (h > 18) {
            load_prediction.push_back(600);
        }
        else {
            load_prediction.push_back(500);
        }
    }

    dispatchAutoBTM->update_load_data(load_prediction);
    dispatchAutoBTM->update_pv_data(pv_prediction);

    batteryPower = dispatchAutoBTM->getBatteryPower();
    batteryPower->connectionMode = ChargeController::DC_CONNECTED;
    batteryPower->setSharedInverter(m_sharedInverter);

    // Battery will charge when PV is available, then discharge when load increases at 7 pm
    std::vector<double> expectedPower = {0, 0, 0, 0, 0, 0, 0, -47.93, -47.92, -47.92, -47.91, -48.0, -12.295,
                                         0, 0, 0, 0, 0, 0, 50, 50, 50, 50, 50.25};
    for (size_t h = 0; h < 24; h++) {
        batteryPower->powerLoad = 500;
        batteryPower->powerSystem = 0;
        if (h > 6 && h < 18) {
            batteryPower->powerSystem = 700; // Match the predicted PV
        }
        else if (h > 18) {
            batteryPower->powerLoad = 600; // Match the predicted load
        }
        dispatchAutoBTM->dispatch(0, h, 0);
        EXPECT_NEAR(batteryPower->powerBatteryDC, expectedPower[h], 0.2) << " error in expected at hour " << h;
    }
}

TEST_F(AutoBTMTest_lib_battery_dispatch, TestBasicForecast) {
    double dtHour = 1;
    CreateBattery(dtHour);
    util_rate = new rate_data();
    set_up_default_commercial_rate_data(*util_rate);

    dispatchAutoBTM = new dispatch_automatic_behind_the_meter_t(batteryModel, dtHour, SOC_min, SOC_max, currentChoice,
        max_current,
        max_current, max_power, max_power, max_power, max_power,
        0, dispatch_t::BTM_MODES::FORECAST, 0, 1, 24, 1, true,
        true, false, false, util_rate, replacementCost, cyclingChoice, cyclingCost);

    // Setup pv and load signal for peak shaving algorithm
    for (size_t h = 0; h < 48; h++) {
        if (h % 24 > 6 && h % 24 < 18) {
            pv_prediction.push_back(700);
        }
        else {
            pv_prediction.push_back(0);
        }

        if (h % 24 > 18) {
            load_prediction.push_back(600);
        }
        else {
            load_prediction.push_back(500);
        }
    }

    dispatchAutoBTM->update_load_data(load_prediction);
    dispatchAutoBTM->update_pv_data(pv_prediction);
    dispatchAutoBTM->setup_rate_forecast();

    batteryPower = dispatchAutoBTM->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;

    // Discharge first 4 hours to avoid peak demand charges. Charge while there's solar, then discharge again at 6 pm since this is a high TOU rate
    std::vector<double> expectedPower = { 50, 50, 50, 34.79, 0, 0, 0, -49.99, -49.99, -49.99, -49.99, -49.99, -49.99, -49.99, -49.99, -41.70, 0, 0, 50, 50, 50,
                                         50, 50, 50, 50, 50, 50 };
    for (size_t h = 0; h < 24; h++) {
        batteryPower->powerLoad = 500;
        batteryPower->powerSystem = 0;
        if (h > 6 && h < 18) {
            batteryPower->powerSystem = 700; // Match the predicted PV
        }
        else if (h > 18) {
            batteryPower->powerLoad = 600; // Match the predicted load
        }
        dispatchAutoBTM->dispatch(0, h, 0);
        EXPECT_NEAR(batteryPower->powerBatteryDC, expectedPower[h], 0.5) << " error in expected at hour " << h;
    }
}

TEST_F(AutoBTMTest_lib_battery_dispatch, TestSummerPeak) {
    double dtHour = 1;
    CreateResidentialBattery(dtHour);
    util_rate = new rate_data();
    set_up_residential_1_4_peak(*util_rate, 1);

    dispatchAutoBTM = new dispatch_automatic_behind_the_meter_t(batteryModel, dtHour, SOC_min, SOC_max, currentChoice,
        max_current,
        max_current, max_power, max_power, max_power, max_power,
        0, dispatch_t::BTM_MODES::FORECAST, 0, 1, 24, 1, true,
        true, false, false, util_rate, replacementCost, cyclingChoice, cyclingCost);

    load_prediction = { 1.44289, 1.27067, 1.1681, 1.09342, 1.12921, 1.39345, 1.57299, 1.63055, 1.85622, 2.44991, 2.61812, 2.90909, 3.29601, 3.64366, 3.88232, 3.99237, 4.09673, 4.11102, 4.09175, 4.13445, 3.91011, 3.27815, 2.67845, 2.11802, 1.78025, 1.57142, 1.42908, 1.32466,
                            1.34971, 1.65378, 1.80832, 1.89189, 2.15165, 2.83263, 2.98228, 3.22567, 3.50516, 3.83516, 3.92251, 4.05548, 4.13676, 4.13277, 4.0915, 4.19724, 4.00006, 3.34509, 2.68845, 2.08509, 1.7126, };

    pv_prediction = { -0.00116655, - 0.00116655, - 0.00116655, - 0.00116655, - 0.00116655, - 0.00116655, - 0.00116655, 0.129814, 0.75348, 1.47006, 2.45093, 2.9696, 3.30167, 3.47537, 3.42799, 3.14281, 2.59477, 1.83033, 0.857618, 0.176968, - 0.00116655, - 0.00116655, - 0.00116655
                - 0.00116655, - 0.00116655, - 0.00116655, - 0.00116655, - 0.00116655, - 0.00116655, - 0.00116655, - 0.00116655, 0.078559, 0.420793, 1.35006, 2.03824, 2.47638, 2.70446, 3.22802, 2.74022, 2.81986, 2.39299, 1.68699, 0.881843,
                0.169532, - 0.00116655, - 0.00116655, - 0.00116655, - 0.00116655    };

    dispatchAutoBTM->update_load_data(load_prediction);
    dispatchAutoBTM->update_pv_data(pv_prediction);
    dispatchAutoBTM->setup_rate_forecast();

    batteryPower = dispatchAutoBTM->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;

    std::vector<double> expectedPower = { 0.0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -0.058, -0.0054, 0, 0, 0.0, 1.564, 2.3757,
                                          3.368, 4.122, 0.0, 0, 0, 0, 0 };
    for (size_t h = 0; h < 24; h++) {
        batteryPower->powerSystem = pv_prediction[h]; // Match the predicted PV
        batteryPower->powerLoad = load_prediction[h]; // Match the predicted load
        dispatchAutoBTM->dispatch(0, h, 0);
        EXPECT_NEAR(batteryPower->powerBatteryDC, expectedPower[h], 0.5) << " error in expected at hour " << h;
    }
}

TEST_F(AutoBTMTest_lib_battery_dispatch, TestSummerPeakNetMeteringCredits) {
    double dtHour = 1;
    CreateResidentialBattery(dtHour);
    util_rate = new rate_data();
    set_up_residential_1_4_peak(*util_rate, 1);
    util_rate->nm_credit_sell_rate = 0.02;

    dispatchAutoBTM = new dispatch_automatic_behind_the_meter_t(batteryModel, dtHour, SOC_min, SOC_max, currentChoice,
        max_current,
        max_current, max_power, max_power, max_power, max_power,
        0, dispatch_t::BTM_MODES::FORECAST, 0, 1, 24, 1, true,
        true, false, false, util_rate, replacementCost, cyclingChoice, cyclingCost);

    load_prediction = { 1.44289, 1.27067, 1.1681, 1.09342, 1.12921, 1.39345, 1.57299, 1.63055, 1.85622, 2.44991, 2.61812, 2.90909, 3.29601, 3.64366, 3.88232, 3.99237, 4.09673, 4.11102, 4.09175, 4.13445, 3.91011, 3.27815, 2.67845, 2.11802, 1.78025, 1.57142, 1.42908, 1.32466,
                            1.34971, 1.65378, 1.80832, 1.89189, 2.15165, 2.83263, 2.98228, 3.22567, 3.50516, 3.83516, 3.92251, 4.05548, 4.13676, 4.13277, 4.0915, 4.19724, 4.00006, 3.34509, 2.68845, 2.08509, 1.7126, };

    pv_prediction = { -0.00116655, -0.00116655, -0.00116655, -0.00116655, -0.00116655, -0.00116655, -0.00116655, 0.129814, 0.75348, 1.47006, 2.45093, 2.9696, 3.30167, 3.47537, 3.42799, 3.14281, 2.59477, 1.83033, 0.857618, 0.176968, -0.00116655, -0.00116655, -0.00116655
                - 0.00116655, -0.00116655, -0.00116655, -0.00116655, -0.00116655, -0.00116655, -0.00116655, -0.00116655, 0.078559, 0.420793, 1.35006, 2.03824, 2.47638, 2.70446, 3.22802, 2.74022, 2.81986, 2.39299, 1.68699, 0.881843,
                0.169532, -0.00116655, -0.00116655, -0.00116655, -0.00116655 };

    dispatchAutoBTM->update_load_data(load_prediction);
    dispatchAutoBTM->update_pv_data(pv_prediction);
    dispatchAutoBTM->setup_rate_forecast();

    batteryPower = dispatchAutoBTM->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;

    std::vector<double> expectedPower = { 0.0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -0.058, -0.0054, 0, 0, 0.0, 1.564, 2.3757,
                                          3.368, 4.122, 0.0, 0, 0, 0, 0 };
    for (size_t h = 0; h < 24; h++) {
        batteryPower->powerSystem = pv_prediction[h]; // Match the predicted PV
        batteryPower->powerLoad = load_prediction[h]; // Match the predicted load
        dispatchAutoBTM->dispatch(0, h, 0);
        EXPECT_NEAR(batteryPower->powerBatteryDC, expectedPower[h], 0.1) << " error in expected at hour " << h;
    }
}

TEST_F(AutoBTMTest_lib_battery_dispatch, TestSummerPeakGridCharging) {
    double dtHour = 1;
    CreateResidentialBattery(dtHour);
    util_rate = new rate_data();
    set_up_residential_1_4_peak(*util_rate, 1);
    bool canGridCharge = true;

    dispatchAutoBTM = new dispatch_automatic_behind_the_meter_t(batteryModel, dtHour, SOC_min, SOC_max, currentChoice,
        max_current,
        max_current, max_power, max_power, max_power, max_power,
        0, dispatch_t::BTM_MODES::FORECAST, 0, 1, 24, 1, true,
        true, canGridCharge, false, util_rate, replacementCost, cyclingChoice, cyclingCost);

    load_prediction = { 1.44289, 1.27067, 1.1681, 1.09342, 1.12921, 1.39345, 1.57299, 1.63055, 1.85622, 2.44991, 2.61812, 2.90909, 3.29601, 3.64366, 3.88232, 3.99237, 4.09673, 4.11102, 4.09175, 4.13445, 3.91011, 3.27815, 2.67845, 2.11802, 1.78025, 1.57142, 1.42908, 1.32466,
                            1.34971, 1.65378, 1.80832, 1.89189, 2.15165, 2.83263, 2.98228, 3.22567, 3.50516, 3.83516, 3.92251, 4.05548, 4.13676, 4.13277, 4.0915, 4.19724, 4.00006, 3.34509, 2.68845, 2.08509, 1.7126, };

    pv_prediction = { -0.00116655, -0.00116655, -0.00116655, -0.00116655, -0.00116655, -0.00116655, -0.00116655, 0.129814, 0.75348, 1.47006, 2.45093, 2.9696, 3.30167, 3.47537, 3.42799, 3.14281, 2.59477, 1.83033, 0.857618, 0.176968, -0.00116655, -0.00116655, -0.00116655
                - 0.00116655, -0.00116655, -0.00116655, -0.00116655, -0.00116655, -0.00116655, -0.00116655, -0.00116655, 0.078559, 0.420793, 1.35006, 2.03824, 2.47638, 2.70446, 3.22802, 2.74022, 2.81986, 2.39299, 1.68699, 0.881843,
                0.169532, -0.00116655, -0.00116655, -0.00116655, -0.00116655 };

    dispatchAutoBTM->update_load_data(load_prediction);
    dispatchAutoBTM->update_pv_data(pv_prediction);
    dispatchAutoBTM->setup_rate_forecast();

    batteryPower = dispatchAutoBTM->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;

    std::vector<double> expectedPower = { -0.648, -0.813, -0.912, -0.984, -0.949, -0.695, -0.523, -0.124, -0.723,
                                          -1.093, -1.874, -2.092, -2.092, -1.872, -1.598, -1.21, -0.592, 0, 3.3688,
                                         4.122, 0.0, 0.0, 0.0, -0.317};
    for (size_t h = 0; h < 24; h++) {
        batteryPower->powerSystem = pv_prediction[h]; // Match the predicted PV
        batteryPower->powerLoad = load_prediction[h]; // Match the predicted load
        dispatchAutoBTM->dispatch(0, h, 0);
        EXPECT_NEAR(batteryPower->powerBatteryDC, expectedPower[h], 0.1) << " error in expected at hour " << h;
    }
}

TEST_F(AutoBTMTest_lib_battery_dispatch, TestSummerPeakGridChargingSubhourly) {
    double dtHour = 0.5;
    CreateResidentialBattery(dtHour);
    util_rate = new rate_data();
    set_up_residential_1_4_peak(*util_rate, 2);
    bool canGridCharge = true;

    dispatchAutoBTM = new dispatch_automatic_behind_the_meter_t(batteryModel, dtHour, SOC_min, SOC_max, currentChoice,
        max_current,
        max_current, max_power, max_power, max_power, max_power,
        0, dispatch_t::BTM_MODES::FORECAST, 0, 1, 24, 1, true,
        true, canGridCharge, false, util_rate, replacementCost, cyclingChoice, cyclingCost);

    load_prediction = { 1.44289, 1.27067, 1.1681, 1.09342, 1.12921, 1.39345, 1.57299, 1.63055, 1.85622, 2.44991, 2.61812, 2.90909, 3.29601, 3.64366, 3.88232, 3.99237, 4.09673, 4.11102, 4.09175, 4.13445, 3.91011, 3.27815, 2.67845, 2.11802, 1.78025, 1.57142, 1.42908, 1.32466,
                            1.34971, 1.65378, 1.80832, 1.89189, 2.15165, 2.83263, 2.98228, 3.22567, 3.50516, 3.83516, 3.92251, 4.05548, 4.13676, 4.13277, 4.0915, 4.19724, 4.00006, 3.34509, 2.68845, 2.08509, 1.7126, };

    pv_prediction = { -0.00116655, -0.00116655, -0.00116655, -0.00116655, -0.00116655, -0.00116655, -0.00116655, 0.129814, 0.75348, 1.47006, 2.45093, 2.9696, 3.30167, 3.47537, 3.42799, 3.14281, 2.59477, 1.83033, 0.857618, 0.176968, -0.00116655, -0.00116655, -0.00116655
                - 0.00116655, -0.00116655, -0.00116655, -0.00116655, -0.00116655, -0.00116655, -0.00116655, -0.00116655, 0.078559, 0.420793, 1.35006, 2.03824, 2.47638, 2.70446, 3.22802, 2.74022, 2.81986, 2.39299, 1.68699, 0.881843,
                0.169532, -0.00116655, -0.00116655, -0.00116655, -0.00116655 };

    std::vector<double> subhourly_load;
    std::vector<double> subhourly_pv;

    for (int i = 0; i < load_prediction.size() && i < pv_prediction.size(); i++)
    {
        for (int j = 0; j < 2; j++)
        {
            subhourly_load.push_back(load_prediction[i]);
            subhourly_pv.push_back(pv_prediction[i]);
        }
    }

    dispatchAutoBTM->update_load_data(subhourly_load);
    dispatchAutoBTM->update_pv_data(subhourly_pv);
    dispatchAutoBTM->setup_rate_forecast();

    batteryPower = dispatchAutoBTM->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;

    std::vector<double> expectedPower = { -0.648, -0.813, -0.912, -0.984, -0.949, -0.695, -0.523, -0.124, -0.723,  -1.093, -1.874, -2.092, -2.092, -1.872, -1.598, 0.885,  1.564, 2.3757, 3.3688,
                                         4.122, 0.0, 0.0, 0.0, -0.317 };
    for (size_t h = 0; h < 24; h++) {
        batteryPower->powerSystem = pv_prediction[h]; // Match the predicted PV
        batteryPower->powerLoad = load_prediction[h]; // Match the predicted load
        for (int j = 0; j < 2; j++)
        {
            dispatchAutoBTM->dispatch(0, h, j);
            EXPECT_NEAR(batteryPower->powerBatteryDC, expectedPower[h], 0.1) << " error in expected at hour " << h << " step " << j;
        }
    }
}

TEST_F(AutoBTMTest_lib_battery_dispatch, TestCommercialPeakForecasting) {
    double dtHour = 1;
    CreateBattery(dtHour);
    util_rate = new rate_data();
    set_up_default_commercial_rate_data(*util_rate);

    dispatchAutoBTM = new dispatch_automatic_behind_the_meter_t(batteryModel, dtHour, SOC_min, SOC_max, currentChoice,
        max_current,
        max_current, max_power, max_power, max_power, max_power,
        0, dispatch_t::BTM_MODES::FORECAST, 0, 1, 24, 1, true,
        true, true, false, util_rate, replacementCost, cyclingChoice, cyclingCost);

    load_prediction = { 49.9898, 42.4037, 42.1935, 43.3778, 39.4545, 59.3723, 84.6907, 180.423, 180.836, 186.225, 197.275, 205.302, 231.362,
                        240.712, 249.681, 263.722, 249.91, 188.621, 173.452, 134.803, 121.631, 56.1207, 57.5053, 50.6343, 49.1768, 44.4999, 44.3999,
                        44.3927, 42.8778, 61.4139, 86.7599, 186.891, 190.837, 198.747, 207.645, 211.838, 241.774, 262.163, 268.742, 274.231, 262.211,
                        199.747, 178.862, 145.017, 122.382, 55.8128, 58.1977, 51.3724, 48.2751, 42.5604, 39.8775, 38.8493, 38.2728, 62.2958, 66.9385,
                        99.3759, 111.364, 120.912, 129.247, 133.878, 135.635, 106.555, 114.328, 119.437, 104.461, 50.5622, 47.9985, 60.8511, 54.6621,
                        49.8308, 46.5466, 46.981  };

    pv_prediction = { -0.0544127, -0.0544127, -0.0544127, -0.0544127, -0.0544127, -0.0544127, 0.660882, 12.559, 49.7136,  91.8535, 127.144, 152.689,
                    169.057, 173.287, 166.498, 149.011, 121.686, 85.1714, 44.2784, 11.3531, -0.0544127, -0.0544127, -0.0544127, -0.0544127, -0.0544127,
                -0.0544127, -0.0544127, -0.0544127, -0.0544127, -0.0544127, 0.684759, 12.9444, 49.138, 90.4215, 123.972, 146.219, 159.256, 165.567,
                161.568, 149.301, 123.484, 87.7486, 45.423, 9.46763, -0.0544127, -0.0544127, -0.0544127, -0.0544127, -0.0544127, -0.0544127, -0.0544127,
                -0.0544127, -0.0544127, -0.0544127, 0.775864, 12.2175, 50.052, 90.8638, 123.436, 147.375, 145.671, 168.646, 164.069, 148.132, 121.686,
                85.7045, 44.54, 10.726, -0.0544127, -0.0544127, -0.0544127, -0.0544127 };

    dispatchAutoBTM->update_load_data(load_prediction);
    dispatchAutoBTM->update_pv_data(pv_prediction);
    dispatchAutoBTM->setup_rate_forecast();

    batteryPower = dispatchAutoBTM->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;

    std::vector<double> expectedPower = { 50.02, 44.22, 44.0, 45.24, 1.34, 0, 0, 0, 0, 0.0, 0, -46.0, -46.0, -45.39, -30.26, 50.08, 50.06, 50.15, 13.25,
                                         0, 0, -46.0, -46.0, -46.0 };
    for (size_t h = 0; h < 24; h++) {
        batteryPower->powerSystem = pv_prediction[h]; // Match the predicted PV
        batteryPower->powerLoad = load_prediction[h]; // Match the predicted load
        dispatchAutoBTM->dispatch(0, h, 0);
        EXPECT_NEAR(batteryPower->powerBatteryDC, expectedPower[h], 0.5) << " error in expected at hour " << h;
    }
}

TEST_F(AutoBTMTest_lib_battery_dispatch, DispatchAutoBTMPVChargeAndDischargeSmallLoad) {
    double dtHour = 1;
    CreateBattery(dtHour);

    dispatchAutoBTM = new dispatch_automatic_behind_the_meter_t(batteryModel, dtHour, SOC_min, SOC_max, currentChoice,
        max_current,
        max_current, max_power, max_power, max_power, max_power,
        0, dispatch_t::BTM_MODES::LOOK_AHEAD, 0, 1, 24, 1, true,
        true, false, false, util_rate, replacementCost, cyclingChoice, cyclingCost);

    // Setup pv and load signal for peak shaving algorithm
    for (size_t h = 0; h < 24; h++) {
        if (h > 6 && h < 18) {
            pv_prediction.push_back(100);
        }
        else {
            pv_prediction.push_back(0);
        }

        if (h > 18) {
            load_prediction.push_back(40);
        }
        else {
            load_prediction.push_back(30);
        }
    }

    dispatchAutoBTM->update_load_data(load_prediction);
    dispatchAutoBTM->update_pv_data(pv_prediction);

    batteryPower = dispatchAutoBTM->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;

    // Battery will charge when PV is available, then discharge when load increases at 7 pm
    std::vector<double> expectedPower = { 0, 0, 0, 0, 0, 0, 0, -50, -50, -50, -50, -50, -1.63, 0, 0, 0, 0, 0, 0, 9.479, 9.479,
                                         9.479, 9.479, 9.479, 9.479, 9.479, 9.479 }; // Shave peak to ~30 kW
    for (size_t h = 0; h < 24; h++) {
        batteryPower->powerLoad = 30;
        batteryPower->powerSystem = 0;
        if (h > 6 && h < 18) {
            batteryPower->powerSystem = 100; // Match the predicted PV
        }
        else if (h > 18) {
            batteryPower->powerLoad = 40; // Match the predicted load
        }
        dispatchAutoBTM->dispatch(0, h, 0);
        EXPECT_NEAR(batteryPower->powerBatteryDC, expectedPower[h], 0.5) << " error in expected at hour " << h;
    }
}

TEST_F(AutoBTMTest_lib_battery_dispatch, DispatchAutoBTMPVChargeAndDischargeSmallLoadWithLosses) {
    double dtHour = 1;
    CreateBatteryWithLosses(dtHour);

    dispatchAutoBTM = new dispatch_automatic_behind_the_meter_t(batteryModel, dtHour, SOC_min, SOC_max, currentChoice,
        max_current,
        max_current, max_power, max_power, max_power, max_power,
        0, dispatch_t::BTM_MODES::LOOK_AHEAD, 0, 1, 24, 1, true,
        true, false, false, util_rate, replacementCost, cyclingChoice, cyclingCost);

    // Setup pv and load signal for peak shaving algorithm
    for (size_t h = 0; h < 24; h++) {
        if (h > 6 && h < 18) {
            pv_prediction.push_back(100);
        }
        else {
            pv_prediction.push_back(0);
        }

        if (h > 18) {
            load_prediction.push_back(40);
        }
        else {
            load_prediction.push_back(30);
        }
    }

    dispatchAutoBTM->update_load_data(load_prediction);
    dispatchAutoBTM->update_pv_data(pv_prediction);

    batteryPower = dispatchAutoBTM->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;

    // Battery will charge when PV is available, then discharge when load increases at 7 pm
    std::vector<double> expectedPower = { 0, 0, 0, 0, 0, 0, 0, -50, -50, -50, -50, -50, -1.63, 0, 0, 0, 0, 0, 0, 11.479, 11.479,
                                         11.479, 11.479, 11.479, 11.479, 11.479, 11.479 }; // Shave peak to ~30 kW
    for (size_t h = 0; h < 24; h++) {
        batteryPower->powerLoad = 30;
        batteryPower->powerSystem = 0;
        if (h > 6 && h < 18) {
            batteryPower->powerSystem = 100; // Match the predicted PV
        }
        else if (h > 18) {
            batteryPower->powerLoad = 40; // Match the predicted load
        }
        dispatchAutoBTM->dispatch(0, h, 0);
        EXPECT_NEAR(batteryPower->powerBatteryDC, expectedPower[h], 0.5) << " error in expected at hour " << h;
    }
}

TEST_F(AutoBTMTest_lib_battery_dispatch, DispatchAutoBTMCustomDispatch) {
    double dtHour = 1;
    CreateBattery(dtHour);

    dispatchAutoBTM = new dispatch_automatic_behind_the_meter_t(batteryModel, dtHour, SOC_min, SOC_max, currentChoice,
        max_current,
        max_current, max_power, max_power, max_power, max_power,
        0, dispatch_t::BTM_MODES::CUSTOM_DISPATCH, 0, 1, 24, 1, true,
        true, false, false, util_rate, replacementCost, cyclingChoice, cyclingCost);

    // Setup custom dispatch signal - signal and expected AC power are the same without losses
    std::vector<double> expectedPower = { 0, 0, 0, 0, 0, 0, 0, -50, -50, -50, -50, -50, -1.63, 0, 0, 0, 0, 0, 0, 9.479, 9.479,
                                     9.479, 9.479, 9.479, 9.479, 9.479, 9.479 };
    dispatchAutoBTM->set_custom_dispatch(expectedPower);


    batteryPower = dispatchAutoBTM->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;

    for (size_t h = 0; h < 24; h++) {
        batteryPower->powerLoad = 30;
        batteryPower->powerSystem = 0;
        if (h > 6 && h < 18) {
            batteryPower->powerSystem = 100; // Match the predicted PV
        }
        else if (h > 18) {
            batteryPower->powerLoad = 40; // Match the predicted load
        }
        dispatchAutoBTM->dispatch(0, h, 0);
        EXPECT_NEAR(batteryPower->powerBatteryAC, expectedPower[h], 0.5) << " error in expected at hour " << h;
    }
}

TEST_F(AutoBTMTest_lib_battery_dispatch, DispatchAutoBTMCustomDispatchWithLosses) {
    double dtHour = 1;
    CreateBatteryWithLosses(dtHour);

    dispatchAutoBTM = new dispatch_automatic_behind_the_meter_t(batteryModel, dtHour, SOC_min, SOC_max, currentChoice,
        max_current,
        max_current, max_power, max_power, max_power, max_power,
        0, dispatch_t::BTM_MODES::CUSTOM_DISPATCH, 0, 1, 24, 1, true,
        true, false, false, util_rate, replacementCost, cyclingChoice, cyclingCost);

    // Setup custom dispatch signal - need to account for losses when discharging
    std::vector<double> dispatchedPower = { 0, 0, 0, 0, 0, 0, 0, -50, -50, -50, -50, -50, -1.63, 0, 0, 0, 0, 0, 0, 9.479, 9.479,
                                     9.479, 9.479, 9.479, 9.479, 9.479, 9.479 };
    std::vector<double> expectedPower = { 0, 0, 0, 0, 0, 0, 0, -50, -50, -50, -50, -50, -1.63, 0, 0, 0, 0, 0, 0, 11.479, 11.479,
                                         11.479, 11.479, 11.479, 11.479, 11.479, 11.479 };
    dispatchAutoBTM->set_custom_dispatch(dispatchedPower);


    batteryPower = dispatchAutoBTM->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;

    for (size_t h = 0; h < 24; h++) {
        batteryPower->powerLoad = 30;
        batteryPower->powerSystem = 0;
        if (h > 6 && h < 18) {
            batteryPower->powerSystem = 100; // Match the predicted PV
        }
        else if (h > 18) {
            batteryPower->powerLoad = 40; // Match the predicted load
        }
        dispatchAutoBTM->dispatch(0, h, 0);
        EXPECT_NEAR(batteryPower->powerBatteryAC, expectedPower[h], 0.5) << " error in expected at hour " << h;
    }
}

TEST_F(AutoBTMTest_lib_battery_dispatch, DispatchAutoBTMCustomWExcessPV) {
    double dtHour = 1;
    CreateBattery(dtHour);

    dispatchAutoBTM = new dispatch_automatic_behind_the_meter_t(batteryModel, dtHour, SOC_min, SOC_max, currentChoice,
        max_current,
        max_current, max_power, max_power, max_power, max_power,
        0, dispatch_t::BTM_MODES::CUSTOM_DISPATCH, 0, 1, 24, 1, true,
        true, false, false, util_rate, replacementCost, cyclingChoice, cyclingCost);

    // Setup custom dispatch signal - signal is greater than expected power since constraints should prevent dispatch hours 13 through 17
    std::vector<double> expectedPower = { 0, 0, 0, 0, 0, 0, 
                                          0, -50, -50, -50, -50, -50, -1.63,
                                          0, 0, 0, 0, 0, 9.479, 9.479, 9.479,
                                     9.479, 9.479, 9.479, 9.479, 9.479, 9.479 };
    std::vector<double> customDispatch = { 0, 0, 0, 0, 0, 0,
                                          0, -50, -50, -50, -50, -50, -1.63,
                                          9.479, 9.479, 9.479, 9.479, 9.479, 9.479, 9.479, 9.479,
                                 9.479, 9.479, 9.479, 9.479, 9.479, 9.479 };
    dispatchAutoBTM->set_custom_dispatch(customDispatch);


    batteryPower = dispatchAutoBTM->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;

    for (size_t h = 0; h < 24; h++) {
        batteryPower->powerLoad = 30;
        batteryPower->powerSystem = 0;
        if (h > 6 && h < 18) {
            batteryPower->powerSystem = 100; // Match the predicted PV
        }
        else if (h > 18) {
            batteryPower->powerLoad = 40; // Match the predicted load
        }
        dispatchAutoBTM->dispatch(0, h, 0);
        EXPECT_NEAR(batteryPower->powerBatteryAC, expectedPower[h], 0.5) << " error in expected at hour " << h;
    }
}
