#include <gtest/gtest.h>

#include "cmod_battery_pvsamv1_test.h"

#include "../input_cases/weather_inputs.h"
#include "../input_cases/pvsamv1_battery_common_data.h"

void daily_battery_stats::compute(std::vector<ssc_number_t> batt_power_data) {
    size_t index = 0;
    size_t n = batt_power_data.size();
    int cycleState = 0; // -1 for charging, 1 for discharging;
    bool halfCycle = false;
    while (index < n) {
        int cycles = 0;
        for (size_t hour = 0; hour < 24 * steps_per_hour; hour++) {
            ssc_number_t currentPower = batt_power_data[index];

            if (fabs(currentPower - 0) < 1e-7) {
                currentPower = 0;
            }

            if (currentPower < 0) {
                if (cycleState != -1) {
                    if (halfCycle) {
                        cycles++;
                        halfCycle = false;
                    }
                    else {
                        halfCycle = true;
                    }
                }
                cycleState = -1;
            }

            if (currentPower > 0) {
                if (cycleState != 1) {
                    if (halfCycle) {
                        cycles++;
                        halfCycle = false;
                    }
                    else {
                        halfCycle = true;
                    }
                }
                cycleState = 1;
            }

            index++;
        }
        if (cycles > peakCycles) {
            peakCycles = cycles;
        }
        avgCycles += cycles;

    }
    ssc_number_t days = n / 24.0 / steps_per_hour;
    avgCycles = avgCycles / days;
    peakKwDischarge = *std::max_element(batt_power_data.begin(), batt_power_data.end());
    peakKwCharge = *std::min_element(batt_power_data.begin(), batt_power_data.end());
}

TEST_F(CMPvsamv1BatteryIntegration_cmod_pvsamv1, TestDailyBatteryStats)
{
    // 48 hrs of battery data to test the compute function
    std::vector<ssc_number_t> batt_power_data = { 0, 1, 0, -1, 0, 2, 0, -2, 0, 3, -3, 4, -1, 6, -4, -1, 0, 0, 1, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 2, 1, 0, 0, 0, -1, -1, -1 };
    EXPECT_EQ(batt_power_data.size(), 48);
    daily_battery_stats batt_stats = daily_battery_stats(batt_power_data);

    EXPECT_EQ(batt_stats.peakKwCharge, -4);
    EXPECT_EQ(batt_stats.peakKwDischarge, 6);
    EXPECT_EQ(batt_stats.peakCycles, 5);
    EXPECT_NEAR(batt_stats.avgCycles, 3, 0.1);
}

/// Test PVSAMv1 with all defaults and battery enabled with 3 automatic dispatch methods
TEST_F(CMPvsamv1BatteryIntegration_cmod_pvsamv1, ResidentialACBatteryModelIntegration)
{
    pvsamv_nofinancial_default(data);
    battery_data_default(data);

    std::map<std::string, double> pairs;
    pairs["en_batt"] = 1;
    pairs["batt_ac_or_dc"] = 1; //AC
    pairs["analysis_period"] = 1;
    set_array(data, "load", load_profile_path, 8760); // Load is required for peak shaving controllers

    ssc_number_t expectedEnergy[3] = { 8594, 8594, 8689 };
    ssc_number_t expectedBatteryChargeEnergy[3] = { 1442, 1443, 258 };
    ssc_number_t expectedBatteryDischargeEnergy[3] = { 1321, 1323, 233 };

    ssc_number_t peakKwCharge[3] = { -2.81, -3.02, -2.25 };
    ssc_number_t peakKwDischarge[3] = { 1.39, 1.30, 0.97 };
    ssc_number_t peakCycles[3] = { 1, 1, 1 };
    ssc_number_t avgCycles[3] = { 1, 0.9973, 0.4904 };

    // Test peak shaving look ahead, peak shaving look behind, and automated grid power target. Others require additional input data
    for (int i = 0; i < 3; i++) {
        pairs["batt_dispatch_choice"] = i;

        int pvsam_errors = modify_ssc_data_and_run_module(data, "pvsamv1", pairs);
        EXPECT_FALSE(pvsam_errors);

        if (!pvsam_errors)
        {
            ssc_number_t annual_energy;
            ssc_data_get_number(data, "annual_energy", &annual_energy);
            EXPECT_NEAR(annual_energy, expectedEnergy[i], m_error_tolerance_hi) << "Annual energy.";

            auto data_vtab = static_cast<var_table*>(data);
            auto annualChargeEnergy = data_vtab->as_vector_ssc_number_t("batt_annual_charge_energy");
            EXPECT_NEAR(annualChargeEnergy[0], expectedBatteryChargeEnergy[i], m_error_tolerance_hi) << "Battery annual charge energy.";

            auto annualDischargeEnergy = data_vtab->as_vector_ssc_number_t("batt_annual_discharge_energy");
            EXPECT_NEAR(annualDischargeEnergy[0], expectedBatteryDischargeEnergy[i], m_error_tolerance_hi) << "Battery annual discharge energy.";

            auto batt_power = data_vtab->as_vector_ssc_number_t("batt_power");
            daily_battery_stats batt_stats = daily_battery_stats(batt_power);

            EXPECT_NEAR(batt_stats.peakKwCharge, peakKwCharge[i], m_error_tolerance_lo);
            EXPECT_NEAR(batt_stats.peakKwDischarge, peakKwDischarge[i], m_error_tolerance_lo);
            EXPECT_NEAR(batt_stats.peakCycles, peakCycles[i], m_error_tolerance_lo);
            EXPECT_NEAR(batt_stats.avgCycles, avgCycles[i], 0.0001);
        }
    }
}

/// Test PVSAMv1 with all defaults and battery enabled with custom dispatch
TEST_F(CMPvsamv1BatteryIntegration_cmod_pvsamv1, ResidentialACDCBatteryModelIntegrationCustomDispatchSparse)
{
    pvsamv_nofinancial_default(data);
    battery_data_default(data);

    std::map<std::string, double> pairs;
    pairs["en_batt"] = 1;

    pairs["analysis_period"] = 1;
    set_array(data, "load", load_profile_path, 8760); // Load is required for peak shaving controllers
    pairs["batt_dispatch_choice"] = 3;
    set_array(data, "batt_custom_dispatch", custom_dispatch_residential_schedule, 8760);

    ssc_number_t expectedEnergy[2] = { 8710, 8717 };
    ssc_number_t expectedBatteryChargeEnergy[2] = { 4.6, 4.7 };
    ssc_number_t expectedBatteryDischargeEnergy[2] = { 0.76, 7.6 };

    ssc_number_t peakKwCharge[2] = { -2.7, -2.8 };
    ssc_number_t peakKwDischarge[2] = { 0.03, 0.16 };
    ssc_number_t peakCycles[2] = { 1, 1 };
    ssc_number_t avgCycles[2] = { 0.0027, 0.0027 };

    // Test both AC and DC using the same dispatch model
    for (int i = 0; i < 2; i++) {
        pairs["batt_ac_or_dc"] = i;

        int pvsam_errors = modify_ssc_data_and_run_module(data, "pvsamv1", pairs);
        EXPECT_FALSE(pvsam_errors);

        if (!pvsam_errors)
        {
            ssc_number_t annual_energy;
            ssc_data_get_number(data, "annual_energy", &annual_energy);
            EXPECT_NEAR(annual_energy, expectedEnergy[i], m_error_tolerance_hi) << "Annual energy.";

            auto data_vtab = static_cast<var_table*>(data);
            auto annualChargeEnergy = data_vtab->as_vector_ssc_number_t("batt_annual_charge_energy");
            EXPECT_NEAR(annualChargeEnergy[0], expectedBatteryChargeEnergy[i], m_error_tolerance_hi) << "Battery annual charge energy.";

            auto annualDischargeEnergy = data_vtab->as_vector_ssc_number_t("batt_annual_discharge_energy");
            EXPECT_NEAR(annualDischargeEnergy[0], expectedBatteryDischargeEnergy[i], m_error_tolerance_hi) << "Battery annual discharge energy.";

            auto batt_power = data_vtab->as_vector_ssc_number_t("batt_power");
            daily_battery_stats batt_stats = daily_battery_stats(batt_power);

            EXPECT_NEAR(batt_stats.peakKwCharge, peakKwCharge[i], m_error_tolerance_lo);
            EXPECT_NEAR(batt_stats.peakKwDischarge, peakKwDischarge[i], m_error_tolerance_lo);
            EXPECT_NEAR(batt_stats.peakCycles, peakCycles[i], m_error_tolerance_lo);
            EXPECT_NEAR(batt_stats.avgCycles, avgCycles[i], 0.0001); // Runs once per year
        }
    }
}

/// Test PVSAMv1 with all defaults and battery enabled with custom dispatch
TEST_F(CMPvsamv1BatteryIntegration_cmod_pvsamv1, ResidentialACDCBatteryModelIntegrationCustomDispatchFull)
{
    pvsamv_nofinancial_default(data);
    battery_data_default(data);

    std::map<std::string, double> pairs;
    pairs["en_batt"] = 1;

    pairs["analysis_period"] = 1;
    set_array(data, "load", load_profile_path, 8760); // Load is required for peak shaving controllers
    pairs["batt_dispatch_choice"] = 3;
    set_array(data, "batt_custom_dispatch", custom_dispatch_residential_hourly_schedule, 8760);

    ssc_number_t expectedEnergy[2] = { 8708, 8672 };
    ssc_number_t expectedBatteryChargeEnergy[2] = { 396.1, 359.95 };
    ssc_number_t expectedBatteryDischargeEnergy[2] = { 395.95, 419.2 };

    ssc_number_t peakKwCharge[2] = { -0.47, -0.46 };
    ssc_number_t peakKwDischarge[2] = { 0.39, 0.41 };
    ssc_number_t peakCycles[2] = { 2, 2 };
    ssc_number_t avgCycles[2] = { 0.8219, 0.8219 };

    // Test both AC and DC using the same dispatch model
    for (int i = 0; i < 2; i++) {
        pairs["batt_ac_or_dc"] = i;

        int pvsam_errors = modify_ssc_data_and_run_module(data, "pvsamv1", pairs);
        EXPECT_FALSE(pvsam_errors);

        if (!pvsam_errors)
        {
            ssc_number_t annual_energy;
            ssc_data_get_number(data, "annual_energy", &annual_energy);
            EXPECT_NEAR(annual_energy, expectedEnergy[i], m_error_tolerance_hi) << "Annual energy for " << i;

            auto data_vtab = static_cast<var_table*>(data);
            auto annualChargeEnergy = data_vtab->as_vector_ssc_number_t("batt_annual_charge_energy");
            EXPECT_NEAR(annualChargeEnergy[0], expectedBatteryChargeEnergy[i], m_error_tolerance_hi) << "Battery annual charge energy for " << i;

            auto annualDischargeEnergy = data_vtab->as_vector_ssc_number_t("batt_annual_discharge_energy");
            EXPECT_NEAR(annualDischargeEnergy[0], expectedBatteryDischargeEnergy[i], m_error_tolerance_hi) << "Battery annual discharge energy for " << i;

            auto batt_power = data_vtab->as_vector_ssc_number_t("batt_power");
            daily_battery_stats batt_stats = daily_battery_stats(batt_power);

            EXPECT_NEAR(batt_stats.peakKwCharge, peakKwCharge[i], m_error_tolerance_lo);
            EXPECT_NEAR(batt_stats.peakKwDischarge, peakKwDischarge[i], m_error_tolerance_lo);
            EXPECT_NEAR(batt_stats.peakCycles, peakCycles[i], m_error_tolerance_lo);
            EXPECT_NEAR(batt_stats.avgCycles, avgCycles[i], 0.0001);
        }
    }
}

/// Test PVSAMv1 with all defaults and battery enabled with manual dispatch
TEST_F(CMPvsamv1BatteryIntegration_cmod_pvsamv1, ResidentialACDCBatteryModelIntegrationManualDispatch)
{
    pvsamv_nofinancial_default(data);
    battery_data_default(data);

    std::map<std::string, double> pairs;
    pairs["en_batt"] = 1;

    pairs["analysis_period"] = 1;
    set_array(data, "load", load_profile_path, 8760); // Load is required for peak shaving controllers
    pairs["batt_dispatch_choice"] = 4;

    ssc_number_t expectedEnergy[2] = { 8701, 8672 };
    ssc_number_t expectedBatteryChargeEnergy[2] = { 468, 488 };
    ssc_number_t expectedBatteryDischargeEnergy[2] = { 437, 446 };

    ssc_number_t peakKwCharge[2] = { -2.37, -2.27 };
    ssc_number_t peakKwDischarge[2] = { 1.31, 1.31 };
    ssc_number_t peakCycles[2] = { 2, 2 };
    ssc_number_t avgCycles[2] = { 0.7178, 0.7205 };

    // Test both AC and DC using the same dispatch model
    for (int i = 0; i < 2; i++) {
        pairs["batt_ac_or_dc"] = i;

        int pvsam_errors = modify_ssc_data_and_run_module(data, "pvsamv1", pairs);
        EXPECT_FALSE(pvsam_errors);

        if (!pvsam_errors)
        {
            ssc_number_t annual_energy;
            ssc_data_get_number(data, "annual_energy", &annual_energy);
            EXPECT_NEAR(annual_energy, expectedEnergy[i], m_error_tolerance_hi) << "Annual energy.";

            auto data_vtab = static_cast<var_table*>(data);
            auto annualChargeEnergy = data_vtab->as_vector_ssc_number_t("batt_annual_charge_energy");
            EXPECT_NEAR(annualChargeEnergy[0], expectedBatteryChargeEnergy[i], m_error_tolerance_hi) << "Battery annual charge energy.";

            auto annualDischargeEnergy = data_vtab->as_vector_ssc_number_t("batt_annual_discharge_energy");
            EXPECT_NEAR(annualDischargeEnergy[0], expectedBatteryDischargeEnergy[i], m_error_tolerance_hi) << "Battery annual discharge energy.";

            auto batt_power = data_vtab->as_vector_ssc_number_t("batt_power");
            daily_battery_stats batt_stats = daily_battery_stats(batt_power);

            EXPECT_NEAR(batt_stats.peakKwCharge, peakKwCharge[i], m_error_tolerance_lo);
            EXPECT_NEAR(batt_stats.peakKwDischarge, peakKwDischarge[i], m_error_tolerance_lo);
            EXPECT_NEAR(batt_stats.peakCycles, peakCycles[i], m_error_tolerance_lo);
            EXPECT_NEAR(batt_stats.avgCycles, avgCycles[i], 0.0001);
        }
    }
}

/// Test PVSAMv1 with all defaults and DC battery enabled with 3 automatic dispatch methods
TEST_F(CMPvsamv1BatteryIntegration_cmod_pvsamv1, ResidentialDCBatteryModelIntegration)
{
    pvsamv_nofinancial_default(data);
    battery_data_default(data);

    std::map<std::string, double> pairs;
    pairs["en_batt"] = 1;
    pairs["batt_ac_or_dc"] = 0; //DC
    pairs["analysis_period"] = 1;
    set_array(data, "load", load_profile_path, 8760); // Load is required for peak shaving controllers

    ssc_number_t expectedEnergy[3] = { 8634, 8637, 8703 };
    ssc_number_t expectedBatteryChargeEnergy[3] = { 1412.75, 1414.89, 253.2 };
    ssc_number_t expectedBatteryDischargeEnergy[3] = { 1283.8, 1285.88, 226.3 };

    ssc_number_t peakKwCharge[3] = { -3.21, -2.96, -2.69 };
    ssc_number_t peakKwDischarge[3] = { 1.40, 1.31, 0.967 };
    ssc_number_t peakCycles[3] = { 2, 2, 1 };
    ssc_number_t avgCycles[3] = { 1.0109, 1.0054, 0.4794 };

    // Test peak shaving look ahead, peak shaving look behind, and automated grid power target. Others require additional input data
    for (int i = 0; i < 3; i++) {
        pairs["batt_dispatch_choice"] = i;

        int pvsam_errors = modify_ssc_data_and_run_module(data, "pvsamv1", pairs);
        EXPECT_FALSE(pvsam_errors);

        if (!pvsam_errors)
        {
            ssc_number_t annual_energy;
            ssc_data_get_number(data, "annual_energy", &annual_energy);
            EXPECT_NEAR(annual_energy, expectedEnergy[i], m_error_tolerance_hi) << "Annual energy.";

            auto data_vtab = static_cast<var_table*>(data);
            auto annualChargeEnergy = data_vtab->as_vector_ssc_number_t("batt_annual_charge_energy");
            EXPECT_NEAR(annualChargeEnergy[0], expectedBatteryChargeEnergy[i], m_error_tolerance_hi) << "Battery annual charge energy.";

            auto annualDischargeEnergy = data_vtab->as_vector_ssc_number_t("batt_annual_discharge_energy");
            EXPECT_NEAR(annualDischargeEnergy[0], expectedBatteryDischargeEnergy[i], m_error_tolerance_hi) << "Battery annual discharge energy.";

            auto batt_power = data_vtab->as_vector_ssc_number_t("batt_power");
            daily_battery_stats batt_stats = daily_battery_stats(batt_power);

            EXPECT_NEAR(batt_stats.peakKwCharge, peakKwCharge[i], m_error_tolerance_lo);
            EXPECT_NEAR(batt_stats.peakKwDischarge, peakKwDischarge[i], m_error_tolerance_lo);
            EXPECT_NEAR(batt_stats.peakCycles, peakCycles[i], m_error_tolerance_lo);
            EXPECT_NEAR(batt_stats.avgCycles, avgCycles[i], 0.0001);
        }
    }
}

/// Test PVSAMv1 with all defaults and battery enabled with 3 automatic dispatch methods
TEST_F(CMPvsamv1BatteryIntegration_cmod_pvsamv1, PPA_ACBatteryModelIntegration)
{
    pvsamv1_pv_defaults(data);
    pvsamv1_battery_defaults(data);
    grid_and_rate_defaults(data);
    singleowner_defaults(data);

    ssc_number_t expectedEnergy[3] = { 37308020, 37307080, 37308021 };
    ssc_number_t expectedBatteryChargeEnergy[3] = { 14779, 24265, 14779 }; // No rate model means battery use is low
    ssc_number_t expectedBatteryDischargeEnergy[3] = { 14663, 23209, 14663 };

    ssc_number_t peakKwCharge[3] = { -1040.2, -1051.5, -1051.5 };
    ssc_number_t peakKwDischarge[3] = { 967.5, 969.5, 969.5 };
    ssc_number_t peakCycles[3] = { 1, 1, 1 };
    ssc_number_t avgCycles[3] = { 0.003, 0.006, 0.003 };

    // Test peak shaving look ahead, peak shaving look behind, and automated grid power target. Others require additional input data
    for (int i = 0; i < 3; i++) {
        ssc_data_set_number(data, "batt_dispatch_choice", i);

        int pvsam_errors = run_pvsam1_battery_ppa(data);
        EXPECT_FALSE(pvsam_errors);

        if (!pvsam_errors)
        {
            ssc_number_t annual_energy;
            ssc_data_get_number(data, "annual_energy", &annual_energy);
            EXPECT_NEAR(annual_energy, expectedEnergy[i], m_error_tolerance_hi) << "Annual energy for " << i;

            auto data_vtab = static_cast<var_table*>(data);
            auto annualChargeEnergy = data_vtab->as_vector_ssc_number_t("batt_annual_charge_energy");
            EXPECT_NEAR(annualChargeEnergy[1], expectedBatteryChargeEnergy[i], m_error_tolerance_hi) << "Battery annual charge energy for " << i;

            auto annualDischargeEnergy = data_vtab->as_vector_ssc_number_t("batt_annual_discharge_energy");
            EXPECT_NEAR(annualDischargeEnergy[1], expectedBatteryDischargeEnergy[i], m_error_tolerance_hi) << "Battery annual discharge energy for " << i;

            auto batt_power = data_vtab->as_vector_ssc_number_t("batt_power");
            daily_battery_stats batt_stats = daily_battery_stats(batt_power);

            EXPECT_NEAR(batt_stats.peakKwCharge, peakKwCharge[i], m_error_tolerance_hi);
            EXPECT_NEAR(batt_stats.peakKwDischarge, peakKwDischarge[i], m_error_tolerance_hi);
            EXPECT_NEAR(batt_stats.peakCycles, peakCycles[i], m_error_tolerance_lo);
            EXPECT_NEAR(batt_stats.avgCycles, avgCycles[i], 0.0001);

            // test temperature
            auto temp_array = data_vtab->as_vector_ssc_number_t("batt_temperature");
            double max_temp = *std::max_element(temp_array.begin(), temp_array.end());
            EXPECT_LT(max_temp, 26);
        }
    }
}

/// Test PVSAMv1 with all defaults and battery enabled with manual dispatch and PPA financial model
TEST_F(CMPvsamv1BatteryIntegration_cmod_pvsamv1, PPA_ManualDispatchBatteryModelIntegration)
{
    pvsamv1_pv_defaults(data);
    pvsamv1_battery_defaults(data);
    grid_and_rate_defaults(data);
    singleowner_defaults(data);

    ssc_number_t expectedEnergy = 37175792;
    ssc_number_t expectedBatteryChargeEnergy = 1298028;
    ssc_number_t expectedBatteryDischargeEnergy = 1165681;

    ssc_number_t peakKwCharge = -1052.0;
    ssc_number_t peakKwDischarge = 846.8;
    ssc_number_t peakCycles = 1;
    ssc_number_t avgCycles = 1;

    ssc_data_set_number(data, "batt_dispatch_choice", 4);

    // Modify utility rate to Salt River Project Super Peak
    ssc_number_t p_ur_ec_sched_weekday_srp[288] = { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6 };
    ssc_data_set_matrix(data, "ur_ec_sched_weekday", p_ur_ec_sched_weekday_srp, 12, 24);
    ssc_number_t p_ur_ec_sched_weekend_srp[288] = { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6 };
    ssc_data_set_matrix(data, "ur_ec_sched_weekend", p_ur_ec_sched_weekend_srp, 12, 24);
    ssc_number_t p_ur_ec_tou_mat_srp[36] = { 1, 1, 9.9999999999999998e+37, 0, 0.2969, 0, 2, 1, 9.9999999999999998e+37, 0, 0.081900000000000001, 0, 3, 1, 9.9999999999999998e+37, 0, 0.34989999999999999, 0, 4, 1, 9.9999999999999998e+37, 0, 0.083599999999999994, 0, 5, 1, 9.9999999999999998e+37, 0, 0.123, 0, 6, 1, 9.9999999999999998e+37, 0, 0.074999999999999997, 0 };
    ssc_data_set_matrix(data, "ur_ec_tou_mat", p_ur_ec_tou_mat_srp, 6, 6);

    int pvsam_errors = run_pvsam1_battery_ppa(data);
    EXPECT_FALSE(pvsam_errors);

    if (!pvsam_errors)
    {
        ssc_number_t annual_energy;
        ssc_data_get_number(data, "annual_energy", &annual_energy);
        EXPECT_NEAR(annual_energy, expectedEnergy, m_error_tolerance_hi) << "Annual energy.";

        auto data_vtab = static_cast<var_table*>(data);
        auto annualChargeEnergy = data_vtab->as_vector_ssc_number_t("batt_annual_charge_energy");
        EXPECT_NEAR(annualChargeEnergy[1], expectedBatteryChargeEnergy, 10) << "Battery annual charge energy.";

        auto annualDischargeEnergy = data_vtab->as_vector_ssc_number_t("batt_annual_discharge_energy");
        EXPECT_NEAR(annualDischargeEnergy[1], expectedBatteryDischargeEnergy, 10) << "Battery annual discharge energy.";

        auto batt_power = data_vtab->as_vector_ssc_number_t("batt_power");
        daily_battery_stats batt_stats = daily_battery_stats(batt_power);

        EXPECT_NEAR(batt_stats.peakKwCharge, peakKwCharge, m_error_tolerance_lo);
        EXPECT_NEAR(batt_stats.peakKwDischarge, peakKwDischarge, m_error_tolerance_lo);
        EXPECT_NEAR(batt_stats.peakCycles, peakCycles, m_error_tolerance_lo);
        EXPECT_NEAR(batt_stats.avgCycles, avgCycles, 0.0001);
    }
}

/// Test PVSAMv1 with all defaults and DC battery enabled with custom dispatch and PPA financial model
TEST_F(CMPvsamv1BatteryIntegration_cmod_pvsamv1, PPA_CustomDispatchBatteryModelDCIntegrationSparse)
{
    pvsamv1_pv_defaults(data);
    pvsamv1_battery_defaults(data);
    grid_and_rate_defaults(data);
    singleowner_defaults(data);

    ssc_number_t expectedEnergy = 37308785;
    ssc_number_t expectedBatteryChargeEnergy = 2040;
    ssc_number_t expectedBatteryDischargeEnergy = 3254.;

    ssc_number_t peakKwCharge = -1020.4;
    ssc_number_t peakKwDischarge = 958.7;
    ssc_number_t peakCycles = 1;
    ssc_number_t avgCycles = 0.0027;

    ssc_data_set_number(data, "batt_dispatch_choice", 3);
    ssc_data_set_number(data, "batt_ac_or_dc", 0);
    set_array(data, "batt_custom_dispatch", custom_dispatch_singleowner_schedule, 8760);

    int pvsam_errors = run_pvsam1_battery_ppa(data);
    EXPECT_FALSE(pvsam_errors);

    if (!pvsam_errors)
    {
        ssc_number_t annual_energy;
        ssc_data_get_number(data, "annual_energy", &annual_energy);
        EXPECT_NEAR(annual_energy, expectedEnergy, m_error_tolerance_hi) << "Annual energy.";

        auto data_vtab = static_cast<var_table*>(data);
        auto annualChargeEnergy = data_vtab->as_vector_ssc_number_t("batt_annual_charge_energy");
        EXPECT_NEAR(annualChargeEnergy[1], expectedBatteryChargeEnergy, m_error_tolerance_hi) << "Battery annual charge energy.";

        auto annualDischargeEnergy = data_vtab->as_vector_ssc_number_t("batt_annual_discharge_energy");
        EXPECT_NEAR(annualDischargeEnergy[1], expectedBatteryDischargeEnergy, m_error_tolerance_hi) << "Battery annual discharge energy.";

        auto batt_power = data_vtab->as_vector_ssc_number_t("batt_power");
        daily_battery_stats batt_stats = daily_battery_stats(batt_power);

        EXPECT_NEAR(batt_stats.peakKwCharge, peakKwCharge, m_error_tolerance_lo);
        EXPECT_NEAR(batt_stats.peakKwDischarge, peakKwDischarge, m_error_tolerance_lo);
        EXPECT_NEAR(batt_stats.peakCycles, peakCycles, m_error_tolerance_lo);
        EXPECT_NEAR(batt_stats.avgCycles, avgCycles, 0.0001); // Runs once per year
    }

}

/// Test PVSAMv1 with all defaults and DC battery enabled with custom dispatch and PPA financial model
TEST_F(CMPvsamv1BatteryIntegration_cmod_pvsamv1, PPA_CustomDispatchBatteryModelDCIntegrationFull)
{
    pvsamv1_pv_defaults(data);
    pvsamv1_battery_defaults(data);
    grid_and_rate_defaults(data);
    singleowner_defaults(data);

    //ssc_number_t expectedEnergy = 37264228;
    ssc_number_t expectedEnergy = 37251482;
    ssc_number_t expectedBatteryChargeEnergy = 419044;
    ssc_number_t expectedBatteryDischargeEnergy = 348966;
    ssc_number_t roundtripEfficiency = 80.6;

    ssc_number_t peakKwCharge = -948.6;
    ssc_number_t peakKwDischarge = 651.7;
    ssc_number_t peakCycles = 3;
    ssc_number_t avgCycles = 1.1945;

    ssc_data_set_number(data, "batt_dispatch_choice", 3);
    ssc_data_set_number(data, "batt_ac_or_dc", 0);
    set_array(data, "batt_custom_dispatch", custom_dispatch_singleowner_hourly_schedule, 8760);

    int pvsam_errors = run_pvsam1_battery_ppa(data);
    EXPECT_FALSE(pvsam_errors);

    if (!pvsam_errors)
    {
        ssc_number_t annual_energy;
        ssc_data_get_number(data, "annual_energy", &annual_energy);
        EXPECT_NEAR(annual_energy, expectedEnergy, m_error_tolerance_hi) << "Annual energy.";

        auto data_vtab = static_cast<var_table*>(data);
        auto annualChargeEnergy = data_vtab->as_vector_ssc_number_t("batt_annual_charge_energy");
        EXPECT_NEAR(annualChargeEnergy[1], expectedBatteryChargeEnergy, m_error_tolerance_hi) << "Battery annual charge energy.";

        auto annualDischargeEnergy = data_vtab->as_vector_ssc_number_t("batt_annual_discharge_energy");
        EXPECT_NEAR(annualDischargeEnergy[1], expectedBatteryDischargeEnergy, m_error_tolerance_hi) << "Battery annual discharge energy.";

        EXPECT_NEAR(data_vtab->lookup("average_battery_roundtrip_efficiency")->num[0], roundtripEfficiency, m_error_tolerance_hi) << "Battery roundtrip efficiency.";

        auto batt_power = data_vtab->as_vector_ssc_number_t("batt_power");
        daily_battery_stats batt_stats = daily_battery_stats(batt_power);

        EXPECT_NEAR(batt_stats.peakKwCharge, peakKwCharge, m_error_tolerance_lo);
        EXPECT_NEAR(batt_stats.peakKwDischarge, peakKwDischarge, m_error_tolerance_lo);
        EXPECT_NEAR(batt_stats.peakCycles, peakCycles, m_error_tolerance_lo);
        EXPECT_NEAR(batt_stats.avgCycles, avgCycles, 0.0001);
    }

}

/// Test PVSAMv1 clipping with multiple subarrays
TEST_F(CMPvsamv1BatteryIntegration_cmod_pvsamv1, CommercialMultipleSubarrayBatteryIntegration)
{
    commercial_multiarray_default(data);

    std::map<std::string, double> pairs;
    pairs["analysis_period"] = 1;

    ssc_number_t expectedEnergy = 537434;
    ssc_number_t expectedBatteryChargeEnergy = 929;
    ssc_number_t expectedBatteryDischargeEnergy = 849;
    ssc_number_t expectedClipLoss = 593.5;

    ssc_number_t peakKwCharge = -10.12;
    ssc_number_t peakKwDischarge = 1.39;
    ssc_number_t peakCycles = 1;
    ssc_number_t avgCycles = 1;

    // Test peak shaving look ahead, peak shaving look behind, and automated grid power target. Others require additional input data
    int pvsam_errors = modify_ssc_data_and_run_module(data, "pvsamv1", pairs);
    EXPECT_FALSE(pvsam_errors);

    if (!pvsam_errors)
    {
        ssc_number_t annual_energy;
        ssc_data_get_number(data, "annual_energy", &annual_energy);
        EXPECT_NEAR(annual_energy, expectedEnergy, m_error_tolerance_hi) << "Annual energy.";

        auto data_vtab = static_cast<var_table*>(data);
        auto annualChargeEnergy = data_vtab->as_vector_ssc_number_t("batt_annual_charge_energy");
        EXPECT_NEAR(annualChargeEnergy[0], expectedBatteryChargeEnergy, m_error_tolerance_hi) << "Battery annual charge energy.";

        auto annualDischargeEnergy = data_vtab->as_vector_ssc_number_t("batt_annual_discharge_energy");
        EXPECT_NEAR(annualDischargeEnergy[0], expectedBatteryDischargeEnergy, m_error_tolerance_hi) << "Battery annual discharge energy.";

        auto dcInverterLoss = data_vtab->as_vector_ssc_number_t("dc_invmppt_loss");
        ssc_number_t totalLoss = 0;
        for (int i = 0; i < dcInverterLoss.size(); i++) {
            totalLoss += dcInverterLoss[i];
        }
        EXPECT_NEAR(totalLoss, expectedClipLoss, m_error_tolerance_lo);

        auto batt_power = data_vtab->as_vector_ssc_number_t("batt_power");
        daily_battery_stats batt_stats = daily_battery_stats(batt_power);

        EXPECT_NEAR(batt_stats.peakKwCharge, peakKwCharge, m_error_tolerance_lo);
        EXPECT_NEAR(batt_stats.peakKwDischarge, peakKwDischarge, m_error_tolerance_lo);
        EXPECT_NEAR(batt_stats.peakCycles, peakCycles, m_error_tolerance_lo);
        EXPECT_NEAR(batt_stats.avgCycles, avgCycles, 0.0001);
    }

}

/// Test Clipping forecast and dispatch
TEST_F(CMPvsamv1BatteryIntegration_cmod_pvsamv1, ClippingForecastTest1_DC_FOM_Dispatch)
{
    commercial_multiarray_default(data);

    std::map<std::string, double> pairs;
    pairs["analysis_period"] = 1;
    pairs["batt_ac_or_dc"] = 0;

    ssc_number_t expectedEnergy = 537030;
    ssc_number_t expectedBatteryChargeEnergy = 929;
    ssc_number_t expectedBatteryDischargeEnergy = 343.96;
    ssc_number_t expectedClipLoss = 593.5;

    ssc_number_t peakKwCharge = -9.488;
    ssc_number_t peakKwDischarge = 1.1;
    ssc_number_t peakCycles = 1;
    ssc_number_t avgCycles = 1;

    // Test peak shaving look ahead, peak shaving look behind, and automated grid power target. Others require additional input data
    int pvsam_errors = modify_ssc_data_and_run_module(data, "pvsamv1", pairs);
    EXPECT_FALSE(pvsam_errors);

    if (!pvsam_errors)
    {
        ssc_number_t annual_energy;
        ssc_data_get_number(data, "annual_energy", &annual_energy);
        EXPECT_NEAR(annual_energy, expectedEnergy, m_error_tolerance_hi) << "Annual energy.";

        auto data_vtab = static_cast<var_table*>(data);
        auto annualChargeEnergy = data_vtab->as_vector_ssc_number_t("batt_annual_charge_energy");
        EXPECT_NEAR(annualChargeEnergy[0], expectedBatteryChargeEnergy, m_error_tolerance_hi) << "Battery annual charge energy.";

        auto annualDischargeEnergy = data_vtab->as_vector_ssc_number_t("batt_annual_discharge_energy");
        EXPECT_NEAR(annualDischargeEnergy[0], expectedBatteryDischargeEnergy, m_error_tolerance_hi) << "Battery annual discharge energy.";

        auto dcInverterLoss = data_vtab->as_vector_ssc_number_t("dc_invmppt_loss");
        ssc_number_t totalLoss = 0;
        for (int i = 0; i < dcInverterLoss.size(); i++) {
            totalLoss += dcInverterLoss[i];
        }
        EXPECT_NEAR(totalLoss, expectedClipLoss, m_error_tolerance_lo);

        auto batt_power = data_vtab->as_vector_ssc_number_t("batt_power");
        daily_battery_stats batt_stats = daily_battery_stats(batt_power);

        EXPECT_NEAR(batt_stats.peakKwCharge, peakKwCharge, m_error_tolerance_lo);
        EXPECT_NEAR(batt_stats.peakKwDischarge, peakKwDischarge, m_error_tolerance_lo);
        EXPECT_NEAR(batt_stats.peakCycles, peakCycles, m_error_tolerance_lo);
        EXPECT_NEAR(batt_stats.avgCycles, avgCycles, 0.0001);
    }

}

TEST_F(CMPvsamv1BatteryIntegration_cmod_pvsamv1, ClippingForecastTest2_DC_FOM_Dispatch_w_forecast)
{
    commercial_multiarray_default(data);

    std::map<std::string, double> pairs;
    pairs["analysis_period"] = 1;
    pairs["batt_ac_or_dc"] = 0;
    set_array(data, "batt_pv_clipping_forecast", clipping_forecast, 8760);

    ssc_number_t expectedEnergy = 537030;
    ssc_number_t expectedBatteryChargeEnergy = 929;
    ssc_number_t expectedBatteryDischargeEnergy = 343.96;
    ssc_number_t expectedClipLoss = 593.5;

    ssc_number_t peakKwCharge = -9.488;
    ssc_number_t peakKwDischarge = 1.1;
    ssc_number_t peakCycles = 1;
    ssc_number_t avgCycles = 1;

    // Test peak shaving look ahead, peak shaving look behind, and automated grid power target. Others require additional input data
    int pvsam_errors = modify_ssc_data_and_run_module(data, "pvsamv1", pairs);
    EXPECT_FALSE(pvsam_errors);

    if (!pvsam_errors)
    {
        ssc_number_t annual_energy;
        ssc_data_get_number(data, "annual_energy", &annual_energy);
        EXPECT_NEAR(annual_energy, expectedEnergy, m_error_tolerance_hi) << "Annual energy.";

        auto data_vtab = static_cast<var_table*>(data);
        auto annualChargeEnergy = data_vtab->as_vector_ssc_number_t("batt_annual_charge_energy");
        EXPECT_NEAR(annualChargeEnergy[0], expectedBatteryChargeEnergy, m_error_tolerance_hi) << "Battery annual charge energy.";

        auto annualDischargeEnergy = data_vtab->as_vector_ssc_number_t("batt_annual_discharge_energy");
        EXPECT_NEAR(annualDischargeEnergy[0], expectedBatteryDischargeEnergy, m_error_tolerance_hi) << "Battery annual discharge energy.";

        auto dcInverterLoss = data_vtab->as_vector_ssc_number_t("dc_invmppt_loss");
        ssc_number_t totalLoss = 0;
        for (int i = 0; i < dcInverterLoss.size(); i++) {
            totalLoss += dcInverterLoss[i];
        }
        EXPECT_NEAR(totalLoss, expectedClipLoss, m_error_tolerance_lo);

        auto batt_power = data_vtab->as_vector_ssc_number_t("batt_power");
        daily_battery_stats batt_stats = daily_battery_stats(batt_power);

        EXPECT_NEAR(batt_stats.peakKwCharge, peakKwCharge, m_error_tolerance_lo);
        EXPECT_NEAR(batt_stats.peakKwDischarge, peakKwDischarge, m_error_tolerance_lo);
        EXPECT_NEAR(batt_stats.peakCycles, peakCycles, m_error_tolerance_lo);
        EXPECT_NEAR(batt_stats.avgCycles, avgCycles, 0.0001);
    }

}

/// Test PVSAMv1 with all defaults and DC battery enabled with custom dispatch and PPA financial model
TEST_F(CMPvsamv1BatteryIntegration_cmod_pvsamv1, PPA_CustomDispatchBatteryModelDCIntegrationFullSubhourly)
{
    pvsamv1_pv_defaults(data);
    pvsamv1_battery_defaults(data);
    grid_and_rate_defaults(data);
    singleowner_defaults(data);

    ssc_number_t expectedEnergy = 37252473;
    ssc_number_t expectedBatteryChargeEnergy = 430570;
    ssc_number_t expectedBatteryDischargeEnergy = 349127;
    ssc_number_t roundtripEfficiency = 80.6;

    ssc_number_t peakKwCharge = -948.6;
    ssc_number_t peakKwDischarge = 651.7;
    ssc_number_t peakCycles = 3;
    ssc_number_t avgCycles = 1.1829;

    ssc_data_set_number(data, "batt_dispatch_choice", 3);
    ssc_data_set_number(data, "batt_ac_or_dc", 0);
    set_array(data, "batt_custom_dispatch", custom_dispatch_singleowner_subhourly_schedule, 8760 * 4);
    set_array(data, "batt_room_temperature_celsius", subhourly_batt_temps, 8760 * 4);
    set_array(data, "dispatch_factors_ts", subhourly_dispatch_factors, 8760 * 4);
    ssc_data_set_string(data, "solar_resource_file", subhourly_weather_file);

    int pvsam_errors = run_pvsam1_battery_ppa(data);
    EXPECT_FALSE(pvsam_errors);

    if (!pvsam_errors)
    {
        double tol = .05;
        ssc_number_t annual_energy;
        ssc_data_get_number(data, "annual_energy", &annual_energy);
        EXPECT_NEAR(annual_energy, expectedEnergy, expectedEnergy * tol) << "Annual energy.";

        auto data_vtab = static_cast<var_table*>(data);
        auto annualChargeEnergy = data_vtab->as_vector_ssc_number_t("batt_annual_charge_energy");
        EXPECT_NEAR(annualChargeEnergy[1], expectedBatteryChargeEnergy, expectedBatteryChargeEnergy * tol) << "Battery annual charge energy.";

        auto annualDischargeEnergy = data_vtab->as_vector_ssc_number_t("batt_annual_discharge_energy");
        EXPECT_NEAR(annualDischargeEnergy[1], expectedBatteryDischargeEnergy, expectedBatteryDischargeEnergy * tol) << "Battery annual discharge energy.";

        EXPECT_NEAR(data_vtab->lookup("average_battery_roundtrip_efficiency")->num[0], roundtripEfficiency, m_error_tolerance_hi) << "Battery roundtrip efficiency.";

        auto batt_power = data_vtab->as_vector_ssc_number_t("batt_power");
        daily_battery_stats batt_stats = daily_battery_stats(batt_power, 4);

        EXPECT_NEAR(batt_stats.peakKwCharge, peakKwCharge, abs(peakKwCharge * 0.01));
        EXPECT_NEAR(batt_stats.peakKwDischarge, peakKwDischarge, peakKwDischarge * 0.01);
        EXPECT_NEAR(batt_stats.peakCycles, peakCycles, m_error_tolerance_lo);
        EXPECT_NEAR(batt_stats.avgCycles, avgCycles, 0.05);
    }

}

TEST_F(CMPvsamv1BatteryIntegration_cmod_pvsamv1, ResidentialDCBatteryModelPriceSignalDispatch)
{
    pvsamv_nofinancial_default(data);
    battery_data_default(data);
    setup_residential_utility_rates(data);

    std::map<std::string, double> pairs;
    pairs["en_batt"] = 1;
    pairs["batt_meter_position"] = 0; // Behind the meter
    pairs["batt_ac_or_dc"] = 0; //DC
    pairs["analysis_period"] = 1;
    set_array(data, "load", load_profile_path, 8760); // Load is required for peak shaving controllers

    ssc_number_t expectedEnergy = 8634;
    ssc_number_t expectedBatteryChargeEnergy = 390.9;
    ssc_number_t expectedBatteryDischargeEnergy = 360.2;

    ssc_number_t peakKwCharge = -3.914;
    ssc_number_t peakKwDischarge = 1.99;
    ssc_number_t peakCycles = 2;
    ssc_number_t avgCycles = 0.41;

    pairs["batt_dispatch_choice"] = 5;

    int pvsam_errors = modify_ssc_data_and_run_module(data, "pvsamv1", pairs);
    EXPECT_FALSE(pvsam_errors);

    if (!pvsam_errors)
    {
        ssc_number_t annual_energy;
        ssc_data_get_number(data, "annual_energy", &annual_energy);
        EXPECT_NEAR(annual_energy, expectedEnergy, m_error_tolerance_hi) << "Annual energy.";

        auto data_vtab = static_cast<var_table*>(data);
        auto annualChargeEnergy = data_vtab->as_vector_ssc_number_t("batt_annual_charge_energy");
        EXPECT_NEAR(annualChargeEnergy[0], expectedBatteryChargeEnergy, m_error_tolerance_hi) << "Battery annual charge energy.";

        auto annualDischargeEnergy = data_vtab->as_vector_ssc_number_t("batt_annual_discharge_energy");
        EXPECT_NEAR(annualDischargeEnergy[0], expectedBatteryDischargeEnergy, m_error_tolerance_hi) << "Battery annual discharge energy.";

        auto batt_power = data_vtab->as_vector_ssc_number_t("batt_power");
        daily_battery_stats batt_stats = daily_battery_stats(batt_power);

        EXPECT_NEAR(batt_stats.peakKwCharge, peakKwCharge, m_error_tolerance_lo);
        EXPECT_NEAR(batt_stats.peakKwDischarge, peakKwDischarge, m_error_tolerance_lo);
        EXPECT_NEAR(batt_stats.peakCycles, peakCycles, m_error_tolerance_lo);
        EXPECT_NEAR(batt_stats.avgCycles, avgCycles, 0.1); // As of 8-26-20 Linux cycles 2 more times in a year than Windows, this changes the NPV by $2 over 25 years

        auto batt_q_rel = data_vtab->as_vector_ssc_number_t("batt_capacity_percent");
        auto batt_cyc_avg = data_vtab->as_vector_ssc_number_t("batt_DOD_cycle_average");
        EXPECT_NEAR(batt_q_rel.back(), 97.846, 2e-2);
        EXPECT_NEAR(batt_cyc_avg.back(), 26.15, 0.5);
    }
}
