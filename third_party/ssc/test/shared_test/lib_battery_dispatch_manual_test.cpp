#include "lib_battery_dispatch_manual_test.h"

size_t year = 0;
size_t hour_of_year = 0;
size_t step_of_hour = 0;

TEST_F(ManualTest_lib_battery_dispatch, PowerLimitsDispatchManualAC) {
    dispatchManual = new dispatch_manual_t(batteryModel, dtHour, SOC_min, SOC_max, currentChoice, currentChargeMax,
                                           currentDischargeMax, powerChargeMax, powerDischargeMax, powerChargeMax,
                                           powerDischargeMax, minimumModeTime,
                                           dispatchChoice, meterPosition, scheduleWeekday, scheduleWeekend, canCharge,
                                           canDischarge, canGridcharge, canGridcharge, percentDischarge,
                                           percentGridcharge);

    batteryPower = dispatchManual->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;
    double powerToFill = dispatchManual->battery_power_to_fill();
    EXPECT_NEAR(dispatchManual->battery_soc(), 50, 0.1);

    // Test max charge power constraint
    batteryPower->powerSystem = 1000;
    batteryPower->voltageSystem = 600;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    EXPECT_NEAR(batteryPower->powerBatteryAC, -powerChargeMax, 2.0);
    EXPECT_LT(dispatchManual->battery_power_to_fill(),
              powerToFill); // Confirm battery power moves in the expected direction

    powerToFill = dispatchManual->battery_power_to_fill();
    // Test max discharge power constraint
    batteryPower->powerSystem = 0;
    batteryPower->voltageSystem = 600;
    batteryPower->powerLoad = 1000;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    EXPECT_NEAR(batteryPower->powerBatteryDC, powerDischargeMax, 2.0);
    EXPECT_GT(dispatchManual->battery_power_to_fill(), powerToFill);
}

TEST_F(ManualTest_lib_battery_dispatch, PowerLimitsDispatchManualDC) {
    dispatchManual = new dispatch_manual_t(batteryModel, dtHour, SOC_min, SOC_max, currentChoice, currentChargeMax,
                                           currentDischargeMax, powerChargeMax, powerDischargeMax, powerChargeMax,
                                           powerDischargeMax, minimumModeTime,
                                           dispatchChoice, meterPosition, scheduleWeekday, scheduleWeekend, canCharge,
                                           canDischarge, canGridcharge, canGridcharge, percentDischarge,
                                           percentGridcharge);

    batteryPower = dispatchManual->getBatteryPower();
    batteryPower->connectionMode = ChargeController::DC_CONNECTED;
    batteryPower->setSharedInverter(m_sharedInverter);

    // Test max charge power constraint
    batteryPower->powerSystem = 1000;
    batteryPower->voltageSystem = 600;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    EXPECT_NEAR(batteryPower->powerBatteryAC, -powerChargeMax, 2.0);

    // Test max discharge power constraint
    batteryPower->powerSystem = 0;
    batteryPower->voltageSystem = 600;
    batteryPower->powerLoad = 1000;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    EXPECT_NEAR(batteryPower->powerBatteryDC, powerDischargeMax, 2.0);
}

TEST_F(ManualTest_lib_battery_dispatch, CurrentLimitsDispatchManualAC) {
    dispatch_t::CURRENT_CHOICE testChoice = dispatch_t::CURRENT_CHOICE::RESTRICT_CURRENT;
    double testChargeMax = 20;
    double testDischargeMax = 20;
    dispatchManual = new dispatch_manual_t(batteryModel, dtHour, SOC_min, SOC_max, testChoice, testChargeMax,
                                           testDischargeMax, powerChargeMax, powerDischargeMax, powerChargeMax,
                                           powerDischargeMax, minimumModeTime,
                                           dispatchChoice, meterPosition, scheduleWeekday, scheduleWeekend, canCharge,
                                           canDischarge, canGridcharge, canGridcharge, percentDischarge,
                                           percentGridcharge);

    batteryPower = dispatchManual->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;

    // Test max charge current constraint
    batteryPower->powerSystem = 1000;
    batteryPower->voltageSystem = 600;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    double current = batteryPower->powerBatteryDC * util::kilowatt_to_watt / batteryPower->voltageSystem;
    EXPECT_NEAR(current, -testChargeMax, 2.0);

    // Test max discharge current constraint
    batteryPower->powerSystem = 0;
    batteryPower->voltageSystem = 600;
    batteryPower->powerLoad = 1000;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    current = batteryPower->powerBatteryDC * util::kilowatt_to_watt / batteryPower->voltageSystem;
    EXPECT_NEAR(current, testDischargeMax, 2.0);
}

TEST_F(ManualTest_lib_battery_dispatch, CurrentLimitsDispatchManualDC) {
    dispatch_t::CURRENT_CHOICE testChoice = dispatch_t::CURRENT_CHOICE::RESTRICT_CURRENT;
    double testChargeMax = 20;
    double testDischargeMax = 20;
    dispatchManual = new dispatch_manual_t(batteryModel, dtHour, SOC_min, SOC_max, testChoice, testChargeMax,
                                           testDischargeMax, powerChargeMax, powerDischargeMax, powerChargeMax,
                                           powerDischargeMax, minimumModeTime,
                                           dispatchChoice, meterPosition, scheduleWeekday, scheduleWeekend, canCharge,
                                           canDischarge, canGridcharge, canGridcharge, percentDischarge,
                                           percentGridcharge);

    batteryPower = dispatchManual->getBatteryPower();
    batteryPower->connectionMode = ChargeController::DC_CONNECTED;
    batteryPower->setSharedInverter(m_sharedInverter);

    // Test max charge constraints
    batteryPower->powerSystem = 1000;
    batteryPower->voltageSystem = 600;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    double current = batteryPower->powerBatteryDC * util::kilowatt_to_watt / batteryPower->voltageSystem;
    EXPECT_NEAR(current, -testChargeMax, 2.0);

    // Test max discharge current constraint
    batteryPower->powerSystem = 0;
    batteryPower->voltageSystem = 600;
    batteryPower->powerLoad = 1000;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    current = batteryPower->powerBatteryDC * util::kilowatt_to_watt / batteryPower->voltageSystem;
    EXPECT_NEAR(current, testDischargeMax, 2.0);
}

TEST_F(ManualTest_lib_battery_dispatch, BothLimitsDispatchManualAC) {
    dispatch_t::CURRENT_CHOICE testChoice = dispatch_t::CURRENT_CHOICE::RESTRICT_BOTH;
    double testChargeMax = 20;
    double testDischargeMax = 20;
    double testDischargeMaxPower = 11; // kW
    double testChargeMaxPower = 12; // kW
    dispatchManual = new dispatch_manual_t(batteryModel, dtHour, SOC_min, SOC_max, testChoice, testChargeMax,
                                           testDischargeMax, testChargeMaxPower, testDischargeMaxPower,
                                           testChargeMaxPower, testDischargeMaxPower, minimumModeTime,
                                           dispatchChoice, meterPosition, scheduleWeekday, scheduleWeekend, canCharge,
                                           canDischarge, canGridcharge, canGridcharge, percentDischarge,
                                           percentGridcharge);

    batteryPower = dispatchManual->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;

    // Test max charge current constraint
    batteryPower->powerSystem = 1000;
    batteryPower->voltageSystem = 600;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    double current = batteryPower->powerBatteryDC * util::kilowatt_to_watt / batteryPower->voltageSystem;
    EXPECT_NEAR(current, -testChargeMax, 2.0);

    hour_of_year += 1;

    // Test max charge power constraint
    batteryPower->powerSystem = 1000;
    batteryPower->voltageSystem = 1200;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    EXPECT_NEAR(batteryPower->powerBatteryAC, -testChargeMaxPower, 2.0);

    hour_of_year += 1;

    // Test max discharge constraints
    batteryPower->powerSystem = 0;
    batteryPower->voltageSystem = 600;
    batteryPower->powerLoad = 1000;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    current = batteryPower->powerBatteryDC * util::kilowatt_to_watt / batteryPower->voltageSystem;
    EXPECT_NEAR(current, testDischargeMax, 2.0);

    hour_of_year += 1;

    // Test max discharge power constraint
    batteryPower->powerSystem = 0;
    batteryPower->voltageSystem = 1200;
    batteryPower->powerLoad = 1000;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    EXPECT_NEAR(batteryPower->powerBatteryAC, testDischargeMaxPower, 2.0);
}

TEST_F(ManualTest_lib_battery_dispatch, BothLimitsDispatchManualDC) {
    dispatch_t::CURRENT_CHOICE testChoice = dispatch_t::CURRENT_CHOICE::RESTRICT_BOTH;
    double testChargeMax = 20; // Amps
    double testDischargeMax = 20; // Amps
    double testDischargeMaxPower = 11; // kW
    double testChargeMaxPower = 11; // kW
    dispatchManual = new dispatch_manual_t(batteryModel, dtHour, SOC_min, SOC_max, testChoice, testChargeMax,
                                           testDischargeMax, testChargeMaxPower, testDischargeMaxPower, powerChargeMax,
                                           powerDischargeMax, minimumModeTime,
                                           dispatchChoice, meterPosition, scheduleWeekday, scheduleWeekend, canCharge,
                                           canDischarge, canGridcharge, canGridcharge, percentDischarge,
                                           percentGridcharge);

    batteryPower = dispatchManual->getBatteryPower();
    batteryPower->connectionMode = ChargeController::DC_CONNECTED;
    batteryPower->setSharedInverter(m_sharedInverter);

    // Test max charge current constraint
    batteryPower->powerSystem = 1000;
    batteryPower->voltageSystem = 600;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    double current = batteryPower->powerBatteryDC * util::kilowatt_to_watt / batteryPower->voltageSystem;
    EXPECT_NEAR(current, -testChargeMax, 2.0);

    hour_of_year += 1;

    // Test max discharge current constraint
    batteryPower->powerSystem = 0;
    batteryPower->voltageSystem = 600;
    batteryPower->powerLoad = 1000;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    current = batteryPower->powerBatteryDC * util::kilowatt_to_watt / batteryPower->voltageSystem;
    EXPECT_NEAR(current, testDischargeMax, 2.0);

    hour_of_year += 1;

    // Test max charge power constraint
    batteryPower->powerSystem = 1000;
    batteryPower->voltageSystem = 1200;
    batteryPower->powerLoad = 0;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    EXPECT_NEAR(batteryPower->powerBatteryDC, -testChargeMaxPower, 2.0);

    hour_of_year += 1;

    // Test max discharge power constraint
    batteryPower->powerSystem = 0;
    batteryPower->voltageSystem = 1200;
    batteryPower->powerLoad = 1000;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    EXPECT_NEAR(batteryPower->powerBatteryDC, testDischargeMaxPower, 2.0);
}

TEST_F(ManualTest_lib_battery_dispatch, DispatchChangeFrequency) {
    double testTimestep = 1.0 / 60.0; // Minute timesteps
    double testMinTime = 4.0; // Only allow dispatch to change every 3 mins
    dispatchManual = new dispatch_manual_t(batteryModel, testTimestep, SOC_min, SOC_max, currentChoice,
                                           currentChargeMax, currentDischargeMax, powerChargeMax, powerDischargeMax,
                                           powerChargeMax, powerDischargeMax, testMinTime,
                                           dispatchChoice, meterPosition, scheduleWeekday, scheduleWeekend, canCharge,
                                           canDischarge, canGridcharge, canGridcharge, percentDischarge,
                                           percentGridcharge);

    batteryPower = dispatchManual->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;

    // Start by charging (0 minutes)
    batteryPower->powerSystem = 1000;
    batteryPower->voltageSystem = 600;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    EXPECT_NEAR(batteryPower->powerBatteryAC, -powerChargeMax, 2.0);

    // Abruptly cut off the PV and increase the load. Power should go to zero (1 minute)
    step_of_hour += 1;
    batteryPower->powerSystem = 0;
    batteryPower->voltageSystem = 600;
    batteryPower->powerLoad = 1000;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    EXPECT_NEAR(batteryPower->powerBatteryDC, 0.0, 0.1);

    // Same status (2nd minute)
    step_of_hour += 1;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    EXPECT_NEAR(batteryPower->powerBatteryDC, 0.0, 0.1);

    // Same status (3rd minute)
    step_of_hour += 1;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    EXPECT_NEAR(batteryPower->powerBatteryDC, 0.0, 0.1);

    // Same status (4th minute)
    step_of_hour += 1;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    EXPECT_NEAR(batteryPower->powerBatteryDC, 0.0, 2.0);

    // Should dispatch to load now (5th minute)
    step_of_hour += 1;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    EXPECT_NEAR(batteryPower->powerBatteryDC, powerChargeMax, 2.0);
}

TEST_F(ManualTest_lib_battery_dispatch, SOCLimitsOnDispatch) {
    hour_of_year = 0; step_of_hour = 0;
    dispatchManual = new dispatch_manual_t(batteryModel, dtHour, SOC_min, SOC_max, currentChoice, currentChargeMax,
                                           currentDischargeMax, powerChargeMax, powerDischargeMax, powerChargeMax,
                                           powerDischargeMax, minimumModeTime,
                                           dispatchChoice, meterPosition, scheduleWeekday, scheduleWeekend, canCharge,
                                           canDischarge, canGridcharge, canGridcharge, percentDischarge,
                                           percentGridcharge);

    batteryPower = dispatchManual->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;
    double soc = dispatchManual->battery_soc();
    EXPECT_NEAR(dispatchManual->battery_soc(), 50, 0.1);

    // Test dispatch iterations to fully charge
    batteryPower->powerSystem = 1000; batteryPower->voltageSystem = 600;
    while (soc < SOC_max && hour_of_year < 100) {
        dispatchManual->dispatch(year, hour_of_year, step_of_hour);
        hour_of_year += 1;
        EXPECT_GT(dispatchManual->battery_soc(), soc);
        soc = dispatchManual->battery_soc();
    }
    EXPECT_NEAR(SOC_max, dispatchManual->battery_soc(), 0.1);
    EXPECT_NEAR(6, hour_of_year, 0.1);

    // Attempt dispatch one more time, should not charge
    hour_of_year += 1;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    EXPECT_NEAR(batteryPower->powerBatteryDC, 0.0, 0.1);

    // Cut off PV and provide load
    batteryPower->powerSystem = 0; batteryPower->voltageSystem = 600; batteryPower->powerLoad = 1000;
    while (soc > SOC_min + tolerance && hour_of_year < 100) {
        dispatchManual->dispatch(year, hour_of_year, step_of_hour);
        hour_of_year += 1;
        soc = dispatchManual->battery_soc();
    }
    EXPECT_NEAR(SOC_min, dispatchManual->battery_soc(), 0.1);
    EXPECT_NEAR(16, hour_of_year, 0.1);

    // Cut off PV and provide load
    batteryPower->powerSystem = 0;
    batteryPower->voltageSystem = 600;
    batteryPower->powerLoad = 1000;
    while (soc > SOC_min + tolerance && hour_of_year < 100) {
        dispatchManual->dispatch(year, hour_of_year, step_of_hour);
        hour_of_year += 1;
        soc = dispatchManual->battery_soc();
    }
    EXPECT_NEAR(SOC_min, dispatchManual->battery_soc(), 0.1);
    EXPECT_NEAR(16, hour_of_year, 0.1);

    // Attempt dispatch one more time, should not discharge
    hour_of_year += 1;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    EXPECT_NEAR(batteryPower->powerBatteryDC, 0.0, 0.1);
}

TEST_F(ManualTest_lib_battery_dispatch, ManualGridChargingOffTest)
{
    // canGridCharge is false by default
    dispatchManual = new dispatch_manual_t(batteryModel, dtHour, SOC_min, SOC_max, currentChoice, currentChargeMax, currentDischargeMax, powerChargeMax, powerDischargeMax, powerChargeMax, powerDischargeMax, minimumModeTime,
                                           dispatchChoice, meterPosition, scheduleWeekday, scheduleWeekend, canCharge, canDischarge, canGridcharge, canGridcharge, percentDischarge, percentGridcharge);

    batteryPower = dispatchManual->getBatteryPower();
    batteryPower->connectionMode = ChargeController::DC_CONNECTED;
    batteryPower->setSharedInverter(m_sharedInverter);

    // Should not charge since grid charging is disallowed
    batteryPower->powerSystem = 0; batteryPower->voltageSystem = 600; batteryPower->powerGridToBattery = 1000;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    EXPECT_NEAR(batteryPower->powerBatteryDC, 0, 0.1);
    EXPECT_NEAR(batteryPower->powerGridToBattery, 0, 0.1);
}

TEST_F(ManualTest_lib_battery_dispatch, ManualGridChargingOnTest)
{
    std::vector<bool> testCanGridcharge;
    std::map<size_t, double> testPercentGridCharge;
    for (int p = 0; p < 6; p++) {
        testCanGridcharge.push_back(1);
        testPercentGridCharge[p] = 100;
    }
    dispatchManual = new dispatch_manual_t(batteryModel, dtHour, SOC_min, SOC_max, currentChoice, currentChargeMax, currentDischargeMax, powerChargeMax, powerDischargeMax, powerChargeMax, powerDischargeMax, minimumModeTime,
                                           dispatchChoice, meterPosition, scheduleWeekday, scheduleWeekend, canCharge, canDischarge, testCanGridcharge, canGridcharge, percentDischarge, testPercentGridCharge);

    batteryPower = dispatchManual->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;
    batteryPower->inverterEfficiencyCutoff = 0;

    // Test grid charging. AC to DC losses come into play
    batteryPower->powerSystem = 0; batteryPower->voltageSystem = 600; batteryPower->powerGridToBattery = 1000;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    EXPECT_NEAR(batteryPower->powerBatteryDC, -powerChargeMax * batteryPower->singlePointEfficiencyACToDC, 1.0);
    EXPECT_NEAR(batteryPower->powerGridToBattery, powerChargeMax, 2.0);
}

TEST_F(ManualTest_lib_battery_dispatch, ManualGridChargingOnDCConnectedTest)
{
    std::vector<bool> testCanGridcharge;
    std::map<size_t, double> testPercentGridCharge;
    for (int p = 0; p < 6; p++) {
        testCanGridcharge.push_back(1);
        testPercentGridCharge[p] = 100;
    }
    dispatchManual = new dispatch_manual_t(batteryModel, dtHour, SOC_min, SOC_max, currentChoice, currentChargeMax, currentDischargeMax, powerChargeMax, powerDischargeMax, powerChargeMax, powerDischargeMax, minimumModeTime,
        dispatchChoice, meterPosition, scheduleWeekday, scheduleWeekend, canCharge, canDischarge, testCanGridcharge, canGridcharge, percentDischarge, testPercentGridCharge);

    batteryPower = dispatchManual->getBatteryPower();
    batteryPower->connectionMode = ChargeController::DC_CONNECTED;
    batteryPower->setSharedInverter(m_sharedInverter);
    batteryPower->inverterEfficiencyCutoff = 0;

    // Test grid charging. AC to DC losses come into play
    batteryPower->powerSystem = 0; batteryPower->voltageSystem = 600; batteryPower->powerGridToBattery = 1000;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    EXPECT_NEAR(batteryPower->powerBatteryDC, -powerChargeMax * batteryPower->singlePointEfficiencyACToDC, 1.0);
    EXPECT_NEAR(batteryPower->powerGridToBattery, powerChargeMax, 2.0);
}

TEST_F(ManualTest_lib_battery_dispatch, NoGridChargingWhilePVIsOnTest)
{
    std::vector<bool> testCanGridcharge;
    std::map<size_t, double> testPercentGridCharge;
    for (int p = 0; p < 6; p++) {
        testCanGridcharge.push_back(1);
        testPercentGridCharge[p] = 100;
    }
    dispatchManual = new dispatch_manual_t(batteryModel, dtHour, SOC_min, SOC_max, currentChoice, currentChargeMax, currentDischargeMax, powerChargeMax, powerDischargeMax, powerChargeMax, powerDischargeMax, minimumModeTime,
                                           dispatchChoice, meterPosition, scheduleWeekday, scheduleWeekend, canCharge, canDischarge, testCanGridcharge, canGridcharge, percentDischarge, testPercentGridCharge);

    batteryPower = dispatchManual->getBatteryPower();
    batteryPower->connectionMode = ChargeController::AC_CONNECTED;

    // Test no grid charging while PV is on
    batteryPower->powerSystem = 1000; batteryPower->voltageSystem = 600; batteryPower->powerGridToBattery = 1000;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    EXPECT_NEAR(batteryPower->powerBatteryAC, -powerChargeMax, 1.0);
    EXPECT_NEAR(batteryPower->powerGridToBattery, 0.0, 2.0);
}

TEST_F(ManualTest_lib_battery_dispatch, EfficiencyLimitsDispatchManualDC)
{
    dispatchManual = new dispatch_manual_t(batteryModel, dtHour, SOC_min, SOC_max, currentChoice, currentChargeMax, currentDischargeMax, powerChargeMax, powerDischargeMax, powerChargeMax, powerDischargeMax, minimumModeTime,
                                           dispatchChoice, meterPosition, scheduleWeekday, scheduleWeekend, canCharge, canDischarge, canGridcharge, canGridcharge, percentDischarge, percentGridcharge);

    batteryPower = dispatchManual->getBatteryPower();
    batteryPower->connectionMode = ChargeController::DC_CONNECTED;
    batteryPower->setSharedInverter(m_sharedInverter);

    // Test max charge power constraint
    batteryPower->powerSystem = 1000; batteryPower->voltageSystem = 600;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    EXPECT_NEAR(batteryPower->powerBatteryAC, -powerChargeMax, 2.0);

    // Test max discharge power constraint
    batteryPower->powerSystem = 0; batteryPower->voltageSystem = 600; batteryPower->powerLoad = 1000;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    EXPECT_NEAR(batteryPower->powerBatteryDC, powerDischargeMax, 2.0);
}

TEST_F(ManualTest_lib_battery_dispatch, InverterEfficiencyCutoffDC)
{
    std::vector<bool> testCanGridcharge;
    std::map<size_t, double> testPercentGridCharge;
    for (int p = 0; p < 6; p++) {
        testCanGridcharge.push_back(1);
        testPercentGridCharge[p] = 1;
    }
    testPercentGridCharge[1] = 1;
    testPercentGridCharge[3] = 100;
    dispatchManual = new dispatch_manual_t(batteryModel, dtHour, SOC_min, SOC_max, currentChoice, currentChargeMax, currentDischargeMax, powerChargeMax, powerDischargeMax, powerChargeMax, powerDischargeMax, minimumModeTime,
                                           dispatchChoice, meterPosition, scheduleWeekday, scheduleWeekend, canCharge, canDischarge, testCanGridcharge, canGridcharge, testPercentGridCharge, testPercentGridCharge);

    batteryPower = dispatchManual->getBatteryPower();
    batteryPower->connectionMode = ChargeController::DC_CONNECTED;
    batteryPower->setSharedInverter(m_sharedInverter);
    batteryPower->inverterEfficiencyCutoff = 80;
    batteryPower->canGridCharge = true;

    // Test inverter efficiency cutoff on grid charging
    batteryPower->powerSystem = 0; batteryPower->voltageSystem = 600; batteryPower->powerGridToBattery = 7;
    dispatchManual->dispatch(year, 0, step_of_hour);
    EXPECT_NEAR(batteryPower->sharedInverter->efficiencyAC, 0.0, 0.1); // Efficiency is 0 when system is not running
    EXPECT_NEAR(batteryPower->powerBatteryDC, 0.0, 0.1);

    batteryPower->powerSystem = 0; batteryPower->voltageSystem = 600; batteryPower->powerGridToBattery = 1000;
    dispatchManual->dispatch(year, 12, step_of_hour);
    EXPECT_NEAR(batteryPower->sharedInverter->efficiencyAC, 93.7, 0.1);
    EXPECT_NEAR(batteryPower->powerBatteryDC, -47.9, 0.1);

    // Test discharge constraints. First constraint does not hit backoff
    batteryPower->powerSystem = 0; batteryPower->voltageSystem = 600; batteryPower->powerGridToBattery = 0; batteryPower->powerLoad = 7;
    dispatchManual->dispatch(year, 0, step_of_hour);
    EXPECT_NEAR(batteryPower->sharedInverter->efficiencyAC, 35.82, 0.1); // Not enforced becasue no PV
    EXPECT_NEAR(batteryPower->powerBatteryDC, 4.43, 0.1);

    batteryPower->powerSystem = 770; batteryPower->voltageSystem = 600; batteryPower->powerGridToBattery = 0; batteryPower->powerLoad = 1000;
    dispatchManual->dispatch(year, 12, step_of_hour);
    EXPECT_NEAR(batteryPower->sharedInverter->efficiencyAC, 93.9, 0.1);
    EXPECT_NEAR(batteryPower->powerBatteryDC, 49.9, 0.1); // Dispatch both battery and PV

    batteryPower->powerSystem = 1000; batteryPower->voltageSystem = 600; batteryPower->powerGridToBattery = 0; batteryPower->powerLoad = 1100;
    dispatchManual->dispatch(year, 12, step_of_hour);
    EXPECT_NEAR(batteryPower->sharedInverter->efficiencyAC, 77, 0.1);
    EXPECT_NEAR(batteryPower->powerBatteryDC, 0.0, 2.0); // Overwhelm inverter with PV, back off battery
}

TEST_F(ManualTest_lib_battery_dispatch_losses, TestLossesWithDispatch)
{
    dispatchManual = new dispatch_manual_t(batteryModel, dtHour, SOC_min, SOC_max, currentChoice, currentChargeMax, currentDischargeMax, powerChargeMax, powerDischargeMax, powerChargeMax, powerDischargeMax, minimumModeTime,
        dispatchChoice, meterPosition, scheduleWeekday, scheduleWeekend, canCharge, canDischarge, canGridcharge, canGridcharge, percentDischarge, percentGridcharge);

    batteryPower = dispatchManual->getBatteryPower();
    batteryPower->connectionMode = ChargeController::DC_CONNECTED;
    batteryPower->setSharedInverter(m_sharedInverter);

    // Test max charge power constraint
    batteryPower->powerSystem = 40; batteryPower->voltageSystem = 600;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    EXPECT_NEAR(batteryPower->powerSystemToBattery, batteryPower->powerSystem - batteryPower->powerSystemLoss, 0.1);

    // Test max discharge power constraint
    batteryPower->powerSystem = 0; batteryPower->voltageSystem = 600; batteryPower->powerLoad = 40;
    dispatchManual->dispatch(year, hour_of_year, step_of_hour);
    EXPECT_NEAR(batteryPower->powerGeneratedBySystem, batteryPower->powerLoad, 0.5); // Constraints drive efficiency lower, meaning some grid power is used to meet load (<0.5 kW)
    EXPECT_NEAR(batteryPower->powerBatteryToLoad, batteryPower->powerLoad, 0.5);
}
