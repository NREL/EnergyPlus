#include <numeric>

#include <gtest/gtest.h>

#include "vartab.h"

#include "cmod_battery_test.h"


/// Test standalone battery compute modeule with a input lifetime generation and commercial load
TEST_F(CMBattery_cmod_battery, CommercialLifetimePeakShaving) {

	// Run with fixed output
	ssc_number_t n_years;
	ssc_data_get_number(data, "analysis_period", &n_years);
	size_t n_lifetime = (size_t)(n_years) * 8760;

	int errors = run_module(data, "battery");
	EXPECT_FALSE(errors);

	if (!errors)
	{
		// roundtrip efficiency test will ensure that the battery cycled
		ssc_number_t roundtripEfficiency;
		ssc_data_get_number(data, "average_battery_roundtrip_efficiency", &roundtripEfficiency);
		EXPECT_NEAR(roundtripEfficiency, 94.42, 2);

		// test that lifetime output is achieved
		int n;
		calculated_array = ssc_data_get_array(data, "gen", &n);
		EXPECT_EQ(n_lifetime, (size_t)n);

		// test that battery was replaced at some point
		calculated_array = ssc_data_get_array(data, "batt_bank_replacement", &n);
		int replacements = std::accumulate(calculated_array, calculated_array + n, 0);
		
		EXPECT_GT(replacements, 0);
	}
}

TEST_F(CMBattery_cmod_battery, ResilienceMetricsFullLoad){
    auto data_vtab = static_cast<var_table*>(data);
    data_vtab->assign("crit_load", data_vtab->as_vector_ssc_number_t("load"));
    data_vtab->assign("system_use_lifetime_output", 0);
    data_vtab->assign("analysis_period", 1);
    data_vtab->assign("gen", var_data(data_vtab->as_array("gen", nullptr), 8760));
    data_vtab->assign("batt_replacement_option", 0);

    int errors = run_module(data, "battery");
    EXPECT_FALSE(errors);

    auto resilience_hours = data_vtab->as_vector_ssc_number_t("resilience_hrs");
    double resilience_hrs_min = data_vtab->as_number("resilience_hrs_min");
    double resilience_hrs_max = data_vtab->as_number("resilience_hrs_max");
    double resilience_hrs_avg = data_vtab->as_number("resilience_hrs_avg");
    auto outage_durations = data_vtab->as_vector_ssc_number_t("outage_durations");
    auto pdf_of_surviving = data_vtab->as_vector_ssc_number_t("pdf_of_surviving");
    double avg_critical_load = data_vtab->as_double("avg_critical_load");

    EXPECT_EQ(resilience_hours[0], 0);
    EXPECT_EQ(resilience_hours[1], 1);
    EXPECT_NEAR(avg_critical_load, 764.73, 0.1);
    EXPECT_NEAR(resilience_hrs_avg, 0.965, 0.01);
    EXPECT_EQ(resilience_hrs_min, 0);
    EXPECT_EQ(outage_durations[0], 0);
    EXPECT_EQ(resilience_hrs_max, 17);
    EXPECT_EQ(outage_durations[17], 17);
    EXPECT_NEAR(pdf_of_surviving[0], 0.704, 1e-3);
    EXPECT_NEAR(pdf_of_surviving[1], 0.066, 1e-3);

    auto batt_power = data_vtab->as_vector_ssc_number_t("batt_power");
    auto power_max = *std::max_element(batt_power.begin(), batt_power.end());
    EXPECT_NEAR(power_max, 166.02, 1e-2);

    std::vector<size_t> max_indices;
    for (size_t i = 0; i < batt_power.size(); i++){
        if (power_max - batt_power[i] < 0.1)
            max_indices.push_back(i);
    }
    EXPECT_EQ(max_indices.size(), 1);
    EXPECT_EQ(max_indices[0], 4351);

    auto batt_q0 = data_vtab->as_vector_ssc_number_t("batt_q0");
    auto cap_max = *std::max_element(batt_q0.begin(), batt_q0.end());
    EXPECT_NEAR(cap_max, 11540.6, 1e-2);

    max_indices.clear();
    for (size_t i = 0; i < batt_q0.size(); i++){
        if (cap_max - batt_q0[i] < 0.01)
            max_indices.push_back(i);
    }
    EXPECT_EQ(max_indices[0], 2);
}

TEST_F(CMBattery_cmod_battery, ResilienceMetricsFullLoadLifetime){
    int nyears = 3;
    auto data_vtab = static_cast<var_table*>(data);
    data_vtab->assign("crit_load", data_vtab->as_vector_ssc_number_t("load"));
    data_vtab->assign("system_use_lifetime_output", 1);
    data_vtab->assign("analysis_period", nyears);
    data_vtab->assign("gen", var_data(data_vtab->as_array("gen", nullptr), 8760 * nyears));
    data_vtab->assign("batt_replacement_option", 0);


    int errors = run_module(data, "battery");
    EXPECT_FALSE(errors);

    auto resilience_hours = data_vtab->as_vector_ssc_number_t("resilience_hrs");
    double resilience_hrs_min = data_vtab->as_number("resilience_hrs_min");
    double resilience_hrs_max = data_vtab->as_number("resilience_hrs_max");
    double resilience_hrs_avg = data_vtab->as_number("resilience_hrs_avg");
    auto outage_durations = data_vtab->as_vector_ssc_number_t("outage_durations");
    auto pdf_of_surviving = data_vtab->as_vector_ssc_number_t("pdf_of_surviving");
    double avg_critical_load = data_vtab->as_double("avg_critical_load");

    EXPECT_EQ(resilience_hours[0], 0);
    EXPECT_EQ(resilience_hours[1], 1);
    EXPECT_NEAR(avg_critical_load, 725.9, 0.1);
    EXPECT_NEAR(resilience_hrs_avg, 0.883, 0.01);
    EXPECT_EQ(resilience_hrs_min, 0);
    EXPECT_EQ(outage_durations[0], 0);
    EXPECT_EQ(resilience_hrs_max, 17);
    EXPECT_EQ(outage_durations[17], 17);
    EXPECT_NEAR(pdf_of_surviving[0], 0.714, 1e-3);
    EXPECT_NEAR(pdf_of_surviving[1], 0.060, 1e-3);

    auto batt_power = data_vtab->as_vector_ssc_number_t("batt_power");
    auto power_max = *std::max_element(batt_power.begin(), batt_power.end());
    EXPECT_NEAR(power_max, 166.02, 1e-2);

    std::vector<size_t> max_indices;
    for (size_t i = 0; i < batt_power.size(); i++){
        if (power_max - batt_power[i] < 0.1)
            max_indices.push_back(i);
    }
    EXPECT_EQ(max_indices[0], 4351);

    auto batt_q0 = data_vtab->as_vector_ssc_number_t("batt_q0");
    auto cap_max = *std::max_element(batt_q0.begin(), batt_q0.end());
    EXPECT_NEAR(cap_max, 11540.6, 1e-2);

    max_indices.clear();
    for (size_t i = 0; i < batt_q0.size(); i++){
        if (cap_max - batt_q0[i] < 0.01)
            max_indices.push_back(i);
    }
    EXPECT_EQ(max_indices[0], 2);
}