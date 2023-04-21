#include <gtest/gtest.h>

#include "../input_cases/code_generator_utilities.h"
#include "vartab.h"
#include "cmod_battwatts_test.h"
#include "battwatts_cases.h"
#include "lib_util.h"

TEST_F(CMBattwatts_cmod_battwatts, ResilienceMetricsHalfLoad){
    CreateData(1);

    auto ssc_dat = static_cast<ssc_data_t>(&data);
    int errors = run_module(ssc_dat, "battwatts");
    EXPECT_FALSE(errors);

    auto resilience_hours = data.as_vector_ssc_number_t("resilience_hrs");
    double resilience_hrs_min = data.as_number("resilience_hrs_min");
    double resilience_hrs_max = data.as_number("resilience_hrs_max");
    double resilience_hrs_avg = data.as_number("resilience_hrs_avg");
    auto outage_durations = data.as_vector_ssc_number_t("outage_durations");
    auto pdf_of_surviving = data.as_vector_ssc_number_t("pdf_of_surviving");
    double avg_critical_load = data.as_double("avg_critical_load");

    EXPECT_EQ(resilience_hours[0], 16);
    EXPECT_EQ(resilience_hours[1], 16);
    EXPECT_NEAR(avg_critical_load, 8.35, 0.1);
    EXPECT_NEAR(resilience_hrs_avg, 32.68, 0.01);
    EXPECT_EQ(resilience_hrs_min, 16);
    EXPECT_EQ(outage_durations[0], 16);
    EXPECT_EQ(resilience_hrs_max, 33);
    EXPECT_EQ(outage_durations[16], 32);
    EXPECT_NEAR(pdf_of_surviving[0], 0.00205, 1e-3);
    EXPECT_NEAR(pdf_of_surviving[1], 0.00217, 1e-3);

}

TEST_F(CMBattwatts_cmod_battwatts, ResilienceMetricsHalfLoadLifetime){
    CreateData(2);

    auto ssc_dat = static_cast<ssc_data_t>(&data);
    int errors = run_module(ssc_dat, "battwatts");
    EXPECT_FALSE(errors);

    auto resilience_hours = data.as_vector_ssc_number_t("resilience_hrs");
    double resilience_hrs_min = data.as_number("resilience_hrs_min");
    double resilience_hrs_max = data.as_number("resilience_hrs_max");
    double resilience_hrs_avg = data.as_number("resilience_hrs_avg");
    auto outage_durations = data.as_vector_ssc_number_t("outage_durations");
    auto pdf_of_surviving = data.as_vector_ssc_number_t("pdf_of_surviving");
    auto cdf_of_surviving = data.as_vector_ssc_number_t("cdf_of_surviving");
    double avg_critical_load = data.as_double("avg_critical_load");

    EXPECT_EQ(resilience_hours[0], 16);
    EXPECT_EQ(resilience_hours[1], 16);
    EXPECT_NEAR(avg_critical_load, 8.39, 0.1);
    EXPECT_NEAR(resilience_hrs_avg, 32.84, 0.01);
    EXPECT_EQ(resilience_hrs_min, 16);
    EXPECT_EQ(resilience_hrs_max, 33);
    EXPECT_EQ(outage_durations[0], 16);
    EXPECT_EQ(outage_durations[16], 32);
    EXPECT_NEAR(pdf_of_surviving[0], 0.00205/2, 1e-5);
    EXPECT_NEAR(pdf_of_surviving[1], 0.00217/2, 1e-5);

}

TEST_F(CMBattwatts_cmod_battwatts, ResidentialDefaults) {
    auto ssc_dat = static_cast<ssc_data_t>(&data);
    pvwatts_pv_defaults(ssc_dat);
    simple_battery_data(ssc_dat);

    int errors = run_module(ssc_dat, "battwatts");
    EXPECT_FALSE(errors);

    double charge_percent = data.as_number("batt_system_charge_percent");
    EXPECT_NEAR(charge_percent, 70.8, 0.1);

    auto batt_power_data = data.as_vector_ssc_number_t("batt_power");
    ssc_number_t peakKwDischarge = *std::max_element(batt_power_data.begin(), batt_power_data.end());
    ssc_number_t peakKwCharge = *std::min_element(batt_power_data.begin(), batt_power_data.end());

    EXPECT_NEAR(peakKwDischarge, 1.97, 0.1);
    EXPECT_NEAR(peakKwCharge, -3.0, 0.1);

    auto batt_voltage = data.as_vector_ssc_number_t("batt_voltage");
    ssc_number_t peakVoltage = *std::max_element(batt_voltage.begin(), batt_voltage.end());
    EXPECT_NEAR(peakVoltage, 578.9, 0.1);

    auto cycles = data.as_vector_ssc_number_t("batt_cycles");
    ssc_number_t maxCycles = *std::max_element(cycles.begin(), cycles.end());
    EXPECT_NEAR(maxCycles, 614, 0.1);
}

TEST_F(CMBattwatts_cmod_battwatts, ResidentialDefaultsLeadAcid) {
    auto ssc_dat = static_cast<ssc_data_t>(&data);
    pvwatts_pv_defaults(ssc_dat);
    simple_battery_data(ssc_dat);

    // Set lead acid
    ssc_data_set_number(ssc_dat, "batt_simple_chemistry", 0);

    int errors = run_module(ssc_dat, "battwatts");
    EXPECT_FALSE(errors);

    double charge_percent = data.as_number("batt_system_charge_percent");
    EXPECT_NEAR(charge_percent, 76.4, 0.1);

    auto batt_power_data = data.as_vector_ssc_number_t("batt_power");
    ssc_number_t peakKwDischarge = *std::max_element(batt_power_data.begin(), batt_power_data.end());
    ssc_number_t peakKwCharge = *std::min_element(batt_power_data.begin(), batt_power_data.end());

    EXPECT_NEAR(peakKwDischarge, 1.83, 0.1);
    EXPECT_NEAR(peakKwCharge, -2.7, 0.1);

    auto batt_voltage = data.as_vector_ssc_number_t("batt_voltage");
    ssc_number_t peakVoltage = *std::max_element(batt_voltage.begin(), batt_voltage.end());
    EXPECT_NEAR(peakVoltage, 61.8, 0.1);

    auto cycles = data.as_vector_ssc_number_t("batt_cycles");
    ssc_number_t maxCycles = *std::max_element(cycles.begin(), cycles.end());
    EXPECT_NEAR(maxCycles, 614, 0.1);
}

TEST_F(CMBattwatts_cmod_battwatts, NoPV) {
    auto ssc_dat = static_cast<ssc_data_t>(&data);
    pvwatts_pv_defaults(ssc_dat);
    simple_battery_data(ssc_dat);

    std::vector<double> ac(8760, 0);
    data.assign("ac", ac);

    int errors = run_module(ssc_dat, "battwatts");
    EXPECT_FALSE(errors);

    double charge_percent = data.as_number("batt_system_charge_percent");
    EXPECT_NEAR(charge_percent, 0.0, 0.1);

    auto batt_power_data = data.as_vector_ssc_number_t("batt_power");
    ssc_number_t peakKwDischarge = *std::max_element(batt_power_data.begin(), batt_power_data.end());
    ssc_number_t peakKwCharge = *std::min_element(batt_power_data.begin(), batt_power_data.end());

    EXPECT_NEAR(peakKwDischarge, 0.9, 0.1);
    EXPECT_NEAR(peakKwCharge, -0.7, 0.1);

    auto batt_voltage = data.as_vector_ssc_number_t("batt_voltage");
    ssc_number_t peakVoltage = *std::max_element(batt_voltage.begin(), batt_voltage.end());
    EXPECT_NEAR(peakVoltage, 573.5, 0.1);

    auto cycles = data.as_vector_ssc_number_t("batt_cycles");
    ssc_number_t maxCycles = *std::max_element(cycles.begin(), cycles.end());
    EXPECT_NEAR(maxCycles, 522, 0.1);
}
