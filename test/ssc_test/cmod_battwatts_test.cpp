#include <gtest/gtest.h>

#include "../input_cases/code_generator_utilities.h"
#include "vartab.h"
#include "cmod_battwatts_test.h"

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
    EXPECT_NEAR(avg_critical_load, 8.06, 0.1);
    EXPECT_NEAR(resilience_hrs_avg, 31.71, 0.01);
    EXPECT_EQ(resilience_hrs_min, 16);
    EXPECT_EQ(outage_durations[0], 16);
    EXPECT_EQ(resilience_hrs_max, 32);
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
    EXPECT_NEAR(avg_critical_load, 8.07, 0.1);
    EXPECT_NEAR(resilience_hrs_avg, 31.86, 0.01);
    EXPECT_EQ(resilience_hrs_min, 16);
    EXPECT_EQ(resilience_hrs_max, 32);
    EXPECT_EQ(outage_durations[0], 16);
    EXPECT_EQ(outage_durations[16], 32);
    EXPECT_NEAR(pdf_of_surviving[0], 0.00205/2, 1e-5);
    EXPECT_NEAR(pdf_of_surviving[1], 0.00217/2, 1e-5);

}