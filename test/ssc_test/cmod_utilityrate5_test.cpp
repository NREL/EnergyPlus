#include <vector>
#include <string>
#include <gtest/gtest.h>

#include "code_generator_utilities.h"
#include "../ssc/cmod_utilityrate5_eqns.h"
#include "cmod_battery_pvsamv1_test.h" // for load profile
#include "vartab.h"
#include "../shared/lib_util.h"

char gen_path[256];
int dummy_for_gen = sprintf(gen_path, "%s/test/input_cases/utility_rate_data/gen_25_year_residential.csv", SSCDIR);

char subhourly_gen_path[256];
int dummy_for_subhourly_gen = sprintf(subhourly_gen_path, "%s/test/input_cases/utility_rate_data/gen_residential_1_year_15_min.csv", SSCDIR);

char commercial_gen_path[256];
int dummy_for_commercial_gen = sprintf(commercial_gen_path, "%s/test/input_cases/utility_rate_data/gen_25_year_commercial.csv", SSCDIR);

char load_commercial[256];
int dummy_for_load = sprintf(load_commercial, "%s/test/input_cases/utility_rate_data/load_commercial.csv", SSCDIR);

char load_residential_subhourly[256];
int dummy_for_subhourly_load = sprintf(load_residential_subhourly, "%s/test/input_cases/pvsamv1_data/pvsamv1_residential_load_15min.csv", SSCDIR);

void setup_residential_rates(ssc_data_t& data) {
    ssc_data_set_number(data, "en_electricity_rates", 1);
    ssc_data_set_number(data, "ur_en_ts_sell_rate", 0);
    ssc_number_t p_ur_ts_buy_rate[1] = { 0 };
    ssc_data_set_array(data, "ur_ts_buy_rate", p_ur_ts_buy_rate, 1);
    ssc_number_t p_ur_ec_sched_weekday[288] = { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4, 4, 4, 4, 4, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4, 4, 4, 4, 4, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4, 4, 4, 4, 4, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4, 4, 4, 4, 4, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4, 4, 4, 4, 4, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4, 4, 4, 4, 4, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 1, 1, 1, 1 };
    ssc_data_set_matrix(data, "ur_ec_sched_weekday", p_ur_ec_sched_weekday, 12, 24);
    ssc_number_t p_ur_ec_sched_weekend[288] = { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 };
    ssc_data_set_matrix(data, "ur_ec_sched_weekend", p_ur_ec_sched_weekend, 12, 24);
    ssc_number_t p_ur_ec_tou_mat[24] = { 1, 1, 9.9999999999999998e+37, 0, 0.10000000000000001, 0,
        2, 1, 9.9999999999999998e+37, 0, 0.050000000000000003, 0,
        3, 1, 9.9999999999999998e+37, 0, 0.20000000000000001, 0,
        4, 1, 9.9999999999999998e+37, 0, 0.25, 0 };
    ssc_data_set_matrix(data, "ur_ec_tou_mat", p_ur_ec_tou_mat, 4, 6);
    ssc_number_t p_ppa_price_input[1] = { 0.089999999999999997 };
    ssc_data_set_array(data, "ppa_price_input", p_ppa_price_input, 1);
    ssc_data_set_number(data, "ppa_multiplier_model", 1);
    ssc_data_set_number(data, "inflation_rate", 2.5);
    ssc_number_t p_degradation[1] = { 0 };
    ssc_data_set_array(data, "degradation", p_degradation, 1);
    ssc_number_t p_rate_escalation[1] = { 0 };
    ssc_data_set_array(data, "rate_escalation", p_rate_escalation, 1);
    ssc_data_set_number(data, "ur_metering_option", 0);
    ssc_data_set_number(data, "ur_nm_yearend_sell_rate", 0.027890000000000002);
    ssc_data_set_number(data, "ur_monthly_fixed_charge", 0);
    ssc_data_set_number(data, "ur_monthly_min_charge", 0);
    ssc_data_set_number(data, "ur_annual_min_charge", 0);
    ssc_number_t  ur_ts_sell_rate[1] = { 0 };
    ssc_data_set_array(data, "ur_ts_sell_rate", ur_ts_sell_rate, 1);
    ssc_data_set_number(data, "ur_dc_enable", 0);
    ssc_number_t p_ur_dc_sched_weekday[288] = { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 };
    ssc_data_set_matrix(data, "ur_dc_sched_weekday", p_ur_dc_sched_weekday, 12, 24);
    ssc_number_t p_ur_dc_sched_weekend[288] = { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 };
    ssc_data_set_matrix(data, "ur_dc_sched_weekend", p_ur_dc_sched_weekend, 12, 24);
    ssc_number_t p_ur_dc_tou_mat[8] = { 1, 1, 9.9999999999999998e+37, 0, 2, 1, 9.9999999999999998e+37, 0 };
    ssc_data_set_matrix(data, "ur_dc_tou_mat", p_ur_dc_tou_mat, 2, 4);
    ssc_number_t p_ur_dc_flat_mat[48] = { 0, 1, 9.9999999999999998e+37, 0, 1, 1, 9.9999999999999998e+37, 0, 2, 1, 9.9999999999999998e+37, 0, 3, 1, 9.9999999999999998e+37, 0, 4, 1, 9.9999999999999998e+37, 0, 5, 1, 9.9999999999999998e+37, 0, 6, 1, 9.9999999999999998e+37, 0, 7, 1, 9.9999999999999998e+37, 0, 8, 1, 9.9999999999999998e+37, 0, 9, 1, 9.9999999999999998e+37, 0, 10, 1, 9.9999999999999998e+37, 0, 11, 1, 9.9999999999999998e+37, 0 };
    ssc_data_set_matrix(data, "ur_dc_flat_mat", p_ur_dc_flat_mat, 12, 4);
};

TEST(URDBv7_cmod_utilityrate5_eqns, ElectricityRates_format_as_URDBv7){
    auto data = new var_table;

    ssc_number_t p_ur_ec_sched_weekday[288] ={ 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 4, 4, 4, 4, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 4, 4, 4, 4 };
    ssc_data_set_matrix( data, "ur_ec_sched_weekday", p_ur_ec_sched_weekday, 12, 24 );
    ssc_number_t p_ur_ec_sched_weekend[288] ={ 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4 };
    ssc_data_set_matrix( data, "ur_ec_sched_weekend", p_ur_ec_sched_weekend, 12, 24 );
    ssc_number_t p_ur_ec_tou_mat[24] ={ 2, 2, 9.9999996802856925e+37, 0, 0.069078996777534485, 0,
                                        2, 1, 100, 0, 0.056908998638391495, 0,
                                        1, 2, 9.9999996802856925e+37, 0, 0.082948997616767883, 0,
                                        1, 1, 100, 0, 0.070768997073173523, 0};
    ssc_data_set_matrix( data, "ur_ec_tou_mat", p_ur_ec_tou_mat, 4, 6 );
    ssc_data_set_number( data, "ur_metering_option", 0 );
    ssc_data_set_number( data, "ur_monthly_fixed_charge", 35.28 );
    ssc_data_set_number( data, "ur_monthly_min_charge", 1 );
    ssc_data_set_number( data, "ur_annual_min_charge", 12 );
    ssc_number_t p_ur_dc_sched_weekday[288] ={ 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2 };
    ssc_data_set_matrix( data, "ur_dc_sched_weekday", p_ur_dc_sched_weekday, 12, 24 );
    ssc_number_t p_ur_dc_sched_weekend[288] ={ 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 };
    ssc_data_set_matrix( data, "ur_dc_sched_weekend", p_ur_dc_sched_weekend, 12, 24 );
    ssc_number_t p_ur_dc_tou_mat[16] ={ 1, 1, 100, 19.538999557495117,
                                        1, 2, 9.9999996802856925e+37, 13.093000411987305,
                                        2, 1, 100, 8.0909996032714844,
                                        2, 2, 9.9999996802856925e+37, 4.6760001182556152 };
    ssc_data_set_matrix( data, "ur_dc_tou_mat", p_ur_dc_tou_mat, 4, 4 );
    ssc_number_t p_ur_dc_flat_mat[52] ={0, 1, 100, 4, 0, 2, 1e+38, 6.46, 1, 1, 1e+38, 6.46, 2, 1, 1e+38, 6.46, 3, 1, 1e+38, 6.46, 4, 1, 1e+38, 13.87, 5, 1, 1e+38, 13.87, 6, 1, 1e+38, 13.87, 7, 1, 1e+38, 13.87, 8, 1, 1e+38, 13.87, 9, 1, 1e+38, 13.87, 10, 1, 1e+38, 6.46, 11, 1, 1e+38, 6.46};
    ssc_data_set_matrix( data, "ur_dc_flat_mat", p_ur_dc_flat_mat, 13, 4 );

    ElectricityRates_format_as_URDBv7(data);

    auto urdb_data = data->lookup("urdb_data")->table;
    auto rules = urdb_data.lookup("dgrules")->str;
    EXPECT_STRCASEEQ(rules.c_str(), "Net Metering");

    auto monthly_fixed = urdb_data.lookup("fixedmonthlycharge")->num;
    EXPECT_NEAR(monthly_fixed, 35.28, 1e-3);

    auto min_charge = urdb_data.lookup("minmonthlycharge")->num;
    EXPECT_NEAR(min_charge, 1, 1e-3);

    min_charge = urdb_data.lookup("annualmincharge")->num;
    EXPECT_NEAR(min_charge, 12, 1e-3);

    auto ec_wd_sched = urdb_data.lookup("energyweekdayschedule")->num.data();
    auto ec_we_sched = urdb_data.lookup("energyweekendschedule")->num.data();
    auto dc_wd_sched = urdb_data.lookup("demandweekdayschedule")->num.data();
    auto dc_we_sched = urdb_data.lookup("demandweekendschedule")->num.data();
    for (size_t i = 0; i < 12 * 24; i++){
        EXPECT_EQ(ec_wd_sched[i], p_ur_ec_sched_weekday[i] - 1);
        EXPECT_EQ(ec_we_sched[i], p_ur_ec_sched_weekend[i] - 1);
        EXPECT_EQ(dc_wd_sched[i], p_ur_dc_sched_weekday[i] - 1);
        EXPECT_EQ(dc_we_sched[i], p_ur_dc_sched_weekend[i] - 1);
    }

    auto dc_flat = urdb_data.lookup("flatdemandmonths")->num;
    std::vector<double> flat_demand_months = {0, 1, 1, 1, 2, 2, 2, 2, 2, 2, 1, 1};
    for (size_t i = 0; i < 12; i++){
        EXPECT_NEAR(dc_flat[i], dc_flat[i], 1e-3);
    }

    auto dc_flat_struct = urdb_data.lookup_match_case("flatdemandstructure")->mat;
    auto period = dc_flat_struct[0];
    EXPECT_NEAR(period[0].table.lookup("max")->num[0], 100, 1e-3);
    EXPECT_NEAR(period[0].table.lookup("rate")->num, 4, 1e-3);
    EXPECT_GT(period[1].table.lookup("max")->num[0], 9.99e+33);
    EXPECT_NEAR(period[1].table.lookup("rate")->num, 6.46, 1e-3);
    period = dc_flat_struct[1];
    EXPECT_GT(period[0].table.lookup("max")->num[0], 9.99e+33);
    EXPECT_NEAR(period[0].table.lookup("rate")->num, 6.46, 1e-3);
    period = dc_flat_struct[2];
    EXPECT_GT(period[0].table.lookup("max")->num[0], 9.99e+33);
    EXPECT_NEAR(period[0].table.lookup("rate")->num, 13.87, 1e-3);

    auto ec_tou_mat = urdb_data.lookup("energyratestructure")->mat;
    period = ec_tou_mat[0];
    EXPECT_NEAR(period[0].table.lookup("max")->num, 100, 1e-3);
    EXPECT_NEAR(period[0].table.lookup("rate")->num, 0.070, 1e-3);
    EXPECT_GT(period[1].table.lookup("max")->num[0], 9.99e+33);
    EXPECT_NEAR(period[1].table.lookup("rate")->num, 0.083, 1e-3);
    period = ec_tou_mat[1];
    EXPECT_NEAR(period[0].table.lookup("max")->num, 100, 1e-3);
    EXPECT_NEAR(period[0].table.lookup("rate")->num, 0.057, 1e-3);
    EXPECT_GT(period[1].table.lookup("max")->num[0], 9.99e+33);
    EXPECT_NEAR(period[1].table.lookup("rate")->num, 0.069, 1e-3);

    auto dc_tou_mat = urdb_data.lookup("demandratestructure")->mat;
    period = dc_tou_mat[0];
    EXPECT_NEAR(period[0].table.lookup("max")->num, 100, 1e-3);
    EXPECT_NEAR(period[0].table.lookup("rate")->num, 19.539, 1e-3);
    EXPECT_GT(period[1].table.lookup("max")->num[0], 9.99e+33);
    EXPECT_NEAR(period[1].table.lookup("rate")->num, 13.093, 1e-3);
    period = dc_tou_mat[1];
    EXPECT_NEAR(period[0].table.lookup("max")->num, 100, 1e-3);
    EXPECT_NEAR(period[0].table.lookup("rate")->num, 8.09, 1e-3);
    EXPECT_GT(period[1].table.lookup("max")->num[0], 9.99e+33);
    EXPECT_NEAR(period[1].table.lookup("rate")->num, 4.676, 1e-3);
}

TEST(cmod_utilityrate5_eqns, Test_Residential_TOU_Rates) {
    ssc_data_t data = new var_table;
    
    setup_residential_rates(data);

    int analysis_period = 25;
    ssc_data_set_number(data, "system_use_lifetime_output", 1);
    ssc_data_set_number(data, "analysis_period", analysis_period);
    set_array(data, "load", load_profile_path, 8760);
    set_array(data, "gen", gen_path, 8760 * analysis_period);

    int status = run_module(data, "utilityrate5");
    EXPECT_FALSE(status);

    ssc_number_t cost_without_system;
    ssc_data_get_number(data, "elec_cost_without_system_year1", &cost_without_system);
    EXPECT_NEAR(771.8, cost_without_system, 0.1);
    
    ssc_number_t cost_with_system;
    ssc_data_get_number(data, "elec_cost_with_system_year1", &cost_with_system);
    EXPECT_NEAR(-11.9, cost_with_system, 0.1);

    int length;
    ssc_number_t* excess_dollars = ssc_data_get_array(data, "year1_nm_dollars_applied", &length);
    float dec_dollars = excess_dollars[length - 1];
    EXPECT_NEAR(75.9, dec_dollars, 0.1);
}

TEST(cmod_utilityrate5_eqns, Test_Residential_net_metering_credits_in_may) {
    ssc_data_t data = new var_table;

    setup_residential_rates(data);

    int analysis_period = 25;
    int credit_month = 4; // May - months index from 0
    ssc_data_set_number(data, "system_use_lifetime_output", 1);
    ssc_data_set_number(data, "ur_nm_credit_month", credit_month);
    ssc_data_set_number(data, "analysis_period", analysis_period);
    set_array(data, "load", load_profile_path, 8760);
    set_array(data, "gen", gen_path, 8760 * analysis_period);

    int status = run_module(data, "utilityrate5");
    EXPECT_FALSE(status);

    ssc_number_t cost_without_system;
    ssc_data_get_number(data, "elec_cost_without_system_year1", &cost_without_system);
    EXPECT_NEAR(771.8, cost_without_system, 0.1);

    ssc_number_t cost_with_system;
    ssc_data_get_number(data, "elec_cost_with_system_year1", &cost_with_system);
    EXPECT_NEAR(36.6, cost_with_system, 0.1);

    int length;
    ssc_number_t* excess_dollars = ssc_data_get_array(data, "year1_nm_dollars_applied", &length);
    float may_dollars = excess_dollars[credit_month];
    EXPECT_NEAR(50.28, may_dollars, 0.1);

    int nrows;
    int ncols;
    ssc_number_t* annual_bills = ssc_data_get_matrix(data, "utility_bill_w_sys_ym", &nrows, &ncols);
    util::matrix_t<double> bill_matrix(nrows, ncols);
    bill_matrix.assign(annual_bills, nrows, ncols);

    double may_year_1 = bill_matrix.at((size_t)1, (size_t)credit_month);
    EXPECT_NEAR(-50.28, may_year_1, 0.1);
}

TEST(cmod_utilityrate5_eqns, Test_Residential_net_metering_credits_in_may_with_rollover) {
    ssc_data_t data = new var_table;

    setup_residential_rates(data);

    int analysis_period = 25;
    int credit_month = 4; // May - months index from 0
    ssc_data_set_number(data, "system_use_lifetime_output", 1);
    ssc_data_set_number(data, "ur_nm_credit_month", credit_month);
    ssc_data_set_number(data, "ur_nm_credit_rollover", 1);
    ssc_data_set_number(data, "analysis_period", analysis_period);
    set_array(data, "load", load_profile_path, 8760);
    set_array(data, "gen", gen_path, 8760 * analysis_period);

    int status = run_module(data, "utilityrate5");
    EXPECT_FALSE(status);

    ssc_number_t cost_without_system;
    ssc_data_get_number(data, "elec_cost_without_system_year1", &cost_without_system);
    EXPECT_NEAR(771.8, cost_without_system, 0.1);

    ssc_number_t cost_with_system;
    ssc_data_get_number(data, "elec_cost_with_system_year1", &cost_with_system);
    EXPECT_NEAR(36.6, cost_with_system, 0.1);

    int length;
    ssc_number_t* excess_dollars = ssc_data_get_array(data, "year1_nm_dollars_applied", &length);
    float may_dollars = excess_dollars[credit_month];
    EXPECT_NEAR(0, may_dollars, 0.1);

    float june_dollars = excess_dollars[credit_month + 1];
    EXPECT_NEAR(11.37, june_dollars, 0.1);

    int nrows;
    int ncols;
    ssc_number_t* annual_bills = ssc_data_get_matrix(data, "utility_bill_w_sys_ym", &nrows, &ncols);
    util::matrix_t<double> bill_matrix(nrows, ncols);
    bill_matrix.assign(annual_bills, nrows, ncols);

    double may_year_1 = bill_matrix.at((size_t)1, (size_t)credit_month);
    EXPECT_NEAR(0.0, may_year_1, 0.1);
}

TEST(cmod_utilityrate5_eqns, Test_Residential_TOU_Rates_subhourly_gen) {
    ssc_data_t data = new var_table;

    setup_residential_rates(data);

    int analysis_period = 1;
    ssc_data_set_number(data, "system_use_lifetime_output", 1);
    ssc_data_set_number(data, "analysis_period", analysis_period);
    set_array(data, "load", load_profile_path, 8760);
    set_array(data, "gen", subhourly_gen_path, 8760 * 4); // 15 min data

    int status = run_module(data, "utilityrate5");
    EXPECT_FALSE(status);

    ssc_number_t cost_without_system;
    ssc_data_get_number(data, "elec_cost_without_system_year1", &cost_without_system);
    EXPECT_NEAR(771.8, cost_without_system, 0.1); // Same as hourly, good!

    ssc_number_t cost_with_system;
    ssc_data_get_number(data, "elec_cost_with_system_year1", &cost_with_system);
    EXPECT_NEAR(-27.94, cost_with_system, 0.1);
}

TEST(cmod_utilityrate5_eqns, Test_Residential_TOU_Rates_subhourly_gen_and_load) {
    ssc_data_t data = new var_table;

    setup_residential_rates(data);

    int analysis_period = 1;
    ssc_data_set_number(data, "system_use_lifetime_output", 1);
    ssc_data_set_number(data, "analysis_period", analysis_period);
    set_array(data, "load", load_residential_subhourly, 8760 * 4); // 15 min data
    set_array(data, "gen", subhourly_gen_path, 8760 * 4); // 15 min data

    int status = run_module(data, "utilityrate5");
    EXPECT_FALSE(status);

    ssc_number_t cost_without_system;
    ssc_data_get_number(data, "elec_cost_without_system_year1", &cost_without_system);
    EXPECT_NEAR(771.8, cost_without_system, 0.1); // Same as hourly, good!

    ssc_number_t cost_with_system;
    ssc_data_get_number(data, "elec_cost_with_system_year1", &cost_with_system);
    EXPECT_NEAR(-27.94, cost_with_system, 0.1);
}

TEST(cmod_utilityrate5_eqns, Test_Residential_TOU_Rates_net_metering_credits) {
    ssc_data_t data = new var_table;

    setup_residential_rates(data);
    ssc_data_set_number(data, "ur_metering_option", 1);

    int analysis_period = 1;
    ssc_data_set_number(data, "system_use_lifetime_output", 1);
    ssc_data_set_number(data, "analysis_period", analysis_period);
    set_array(data, "load", load_profile_path, 8760);
    set_array(data, "gen", subhourly_gen_path, 8760 * 4); // 15 min data

    int status = run_module(data, "utilityrate5");
    EXPECT_FALSE(status);

    ssc_number_t cost_without_system;
    ssc_data_get_number(data, "elec_cost_without_system_year1", &cost_without_system);
    EXPECT_NEAR(771.8, cost_without_system, 0.1); // Same as hourly, good!

    ssc_number_t cost_with_system;
    ssc_data_get_number(data, "elec_cost_with_system_year1", &cost_with_system);
    EXPECT_NEAR(81.4, cost_with_system, 0.1); 
}

TEST(cmod_utilityrate5_eqns, Test_Residential_TOU_Rates_net_billing) {
    ssc_data_t data = new var_table;

    setup_residential_rates(data);
    ssc_data_set_number(data, "ur_metering_option", 2);

    int analysis_period = 1;
    ssc_data_set_number(data, "system_use_lifetime_output", 1);
    ssc_data_set_number(data, "analysis_period", analysis_period);
    set_array(data, "load", load_profile_path, 8760);
    set_array(data, "gen", subhourly_gen_path, 8760 * 4); // 15 min data

    int status = run_module(data, "utilityrate5");
    EXPECT_FALSE(status);

    ssc_number_t cost_without_system;
    ssc_data_get_number(data, "elec_cost_without_system_year1", &cost_without_system);
    EXPECT_NEAR(771.8, cost_without_system, 0.1); // Same as hourly, good!

    ssc_number_t cost_with_system;
    ssc_data_get_number(data, "elec_cost_with_system_year1", &cost_with_system);
    EXPECT_NEAR(441.4, cost_with_system, 0.1);
}

TEST(cmod_utilityrate5_eqns, Test_Residential_TOU_Rates_net_billing_carryover) {
    ssc_data_t data = new var_table;

    setup_residential_rates(data);
    ssc_data_set_number(data, "ur_metering_option", 3);
    ssc_number_t p_ur_ec_tou_mat[24] = { 1, 1, 9.9999999999999998e+37, 0, 0.10000000000000001, 0.10000000000000001,
                                     2, 1, 9.9999999999999998e+37, 0, 0.050000000000000003, 0.050000000000000003,
                                     3, 1, 9.9999999999999998e+37, 0, 0.20000000000000001, 0.20000000000000001,
                                     4, 1, 9.9999999999999998e+37, 0, 0.25, 0.25 };
    ssc_data_set_matrix(data, "ur_ec_tou_mat", p_ur_ec_tou_mat, 4, 6);

    int analysis_period = 25;
    ssc_data_set_number(data, "system_use_lifetime_output", 1);
    ssc_data_set_number(data, "analysis_period", analysis_period);
    set_array(data, "load", load_profile_path, 8760);
    set_array(data, "gen", gen_path, 8760 * analysis_period);

    int status = run_module(data, "utilityrate5");
    EXPECT_FALSE(status);

    ssc_number_t cost_without_system;
    ssc_data_get_number(data, "elec_cost_without_system_year1", &cost_without_system);
    EXPECT_NEAR(771.8, cost_without_system, 0.1); 

    ssc_number_t cost_with_system;
    ssc_data_get_number(data, "elec_cost_with_system_year1", &cost_with_system);
    EXPECT_NEAR(-184.5, cost_with_system, 0.1);

    int nrows;
    int ncols;
    ssc_number_t* annual_bills = ssc_data_get_matrix(data, "utility_bill_w_sys_ym", &nrows, &ncols);
    util::matrix_t<double> bill_matrix(nrows, ncols);
    bill_matrix.assign(annual_bills, nrows, ncols);

    double jan_year_2 = bill_matrix.at((size_t) 2, (size_t) 0);
    EXPECT_NEAR(32.54, jan_year_2, 0.1);
}

TEST(cmod_utilityrate5_eqns, Test_Residential_TOU_Rates_net_billing_carryover_april_reset) {
    ssc_data_t data = new var_table;

    setup_residential_rates(data);
    ssc_data_set_number(data, "ur_metering_option", 3);
    ssc_number_t p_ur_ec_tou_mat[24] = { 1, 1, 9.9999999999999998e+37, 0, 0.10000000000000001, 0.10000000000000001,
                                     2, 1, 9.9999999999999998e+37, 0, 0.050000000000000003, 0.050000000000000003,
                                     3, 1, 9.9999999999999998e+37, 0, 0.20000000000000001, 0.20000000000000001,
                                     4, 1, 9.9999999999999998e+37, 0, 0.25, 0.25 };
    ssc_data_set_matrix(data, "ur_ec_tou_mat", p_ur_ec_tou_mat, 4, 6);
    ssc_data_set_number(data, "ur_nm_credit_month", 3); // April

    int analysis_period = 25;
    ssc_data_set_number(data, "system_use_lifetime_output", 1);
    ssc_data_set_number(data, "analysis_period", analysis_period);
    set_array(data, "load", load_profile_path, 8760);
    set_array(data, "gen", gen_path, 8760 * analysis_period);

    int status = run_module(data, "utilityrate5");
    EXPECT_FALSE(status);

    ssc_number_t cost_without_system;
    ssc_data_get_number(data, "elec_cost_without_system_year1", &cost_without_system);
    EXPECT_NEAR(771.8, cost_without_system, 0.1);

    ssc_number_t cost_with_system;
    ssc_data_get_number(data, "elec_cost_with_system_year1", &cost_with_system);
    EXPECT_NEAR(-173.7, cost_with_system, 0.1);

    int nrows;
    int ncols;
    ssc_number_t* annual_bills = ssc_data_get_matrix(data, "utility_bill_w_sys_ym", &nrows, &ncols);
    util::matrix_t<double> bill_matrix(nrows, ncols);
    bill_matrix.assign(annual_bills, nrows, ncols);

    double jan_year_2 = bill_matrix.at((size_t)2, (size_t)0);
    EXPECT_NEAR(0.0, jan_year_2, 0.1);
}

TEST(cmod_utilityrate5_eqns, Test_Residential_TOU_Rates_net_billing_carryover_jan_reset) {
    ssc_data_t data = new var_table;

    setup_residential_rates(data);
    ssc_data_set_number(data, "ur_metering_option", 3);
    ssc_number_t p_ur_ec_tou_mat[24] = { 1, 1, 9.9999999999999998e+37, 0, 0.10000000000000001, 0.10000000000000001,
                                     2, 1, 9.9999999999999998e+37, 0, 0.050000000000000003, 0.050000000000000003,
                                     3, 1, 9.9999999999999998e+37, 0, 0.20000000000000001, 0.20000000000000001,
                                     4, 1, 9.9999999999999998e+37, 0, 0.25, 0.25 };
    ssc_data_set_matrix(data, "ur_ec_tou_mat", p_ur_ec_tou_mat, 4, 6);
    ssc_data_set_number(data, "ur_nm_credit_month", 0); // January

    int analysis_period = 25;
    ssc_data_set_number(data, "system_use_lifetime_output", 1);
    ssc_data_set_number(data, "analysis_period", analysis_period);
    set_array(data, "load", load_profile_path, 8760);
    set_array(data, "gen", gen_path, 8760 * analysis_period);

    int status = run_module(data, "utilityrate5");
    EXPECT_FALSE(status);

    ssc_number_t cost_without_system;
    ssc_data_get_number(data, "elec_cost_without_system_year1", &cost_without_system);
    EXPECT_NEAR(771.8, cost_without_system, 0.1);

    ssc_number_t cost_with_system;
    ssc_data_get_number(data, "elec_cost_with_system_year1", &cost_with_system);
    EXPECT_NEAR(-153.91, cost_with_system, 0.1);

    int nrows;
    int ncols;
    ssc_number_t* annual_bills = ssc_data_get_matrix(data, "utility_bill_w_sys_ym", &nrows, &ncols);
    util::matrix_t<double> bill_matrix(nrows, ncols);
    bill_matrix.assign(annual_bills, nrows, ncols);

    // Receive rollover credits in January
    double jan_year_2 = bill_matrix.at((size_t)2, (size_t)0);
    EXPECT_NEAR(-40.99, jan_year_2, 0.1);
}

TEST(cmod_utilityrate5_eqns, Test_Residential_TOU_Rates_net_billing_carryover_incorrect_month) {
    ssc_data_t data = new var_table;

    setup_residential_rates(data);
    ssc_data_set_number(data, "ur_metering_option", 3);
    ssc_number_t p_ur_ec_tou_mat[24] = { 1, 1, 9.9999999999999998e+37, 0, 0.10000000000000001, 0.10000000000000001,
                                     2, 1, 9.9999999999999998e+37, 0, 0.050000000000000003, 0.050000000000000003,
                                     3, 1, 9.9999999999999998e+37, 0, 0.20000000000000001, 0.20000000000000001,
                                     4, 1, 9.9999999999999998e+37, 0, 0.25, 0.25 };
    ssc_data_set_matrix(data, "ur_ec_tou_mat", p_ur_ec_tou_mat, 4, 6);
    ssc_data_set_number(data, "ur_nm_credit_month", 12); // Non-existant month

    int analysis_period = 25;
    ssc_data_set_number(data, "system_use_lifetime_output", 1);
    ssc_data_set_number(data, "analysis_period", analysis_period);
    set_array(data, "load", load_profile_path, 8760);
    set_array(data, "gen", gen_path, 8760 * analysis_period);

    int status = run_module(data, "utilityrate5");
    EXPECT_TRUE(status);
}

TEST(cmod_utilityrate5_eqns, Test_Residential_TOU_Rates_buyall_sellall) {
    ssc_data_t data = new var_table;

    setup_residential_rates(data);
    ssc_data_set_number(data, "ur_metering_option", 4);
    // Need to specify sell rates for this configuration. These are higher than real life
    ssc_number_t p_ur_ec_tou_mat[24] = { 1, 1, 9.9999999999999998e+37, 0, 0.10000000000000001, 0.10000000000000001, 
                                         2, 1, 9.9999999999999998e+37, 0, 0.050000000000000003, 0.050000000000000003, 
                                         3, 1, 9.9999999999999998e+37, 0, 0.20000000000000001, 0.20000000000000001, 
                                         4, 1, 9.9999999999999998e+37, 0, 0.25, 0.25 };
    ssc_data_set_matrix(data, "ur_ec_tou_mat", p_ur_ec_tou_mat, 4, 6);

    int analysis_period = 1;
    ssc_data_set_number(data, "system_use_lifetime_output", 1);
    ssc_data_set_number(data, "analysis_period", analysis_period);
    set_array(data, "load", load_profile_path, 8760);
    set_array(data, "gen", subhourly_gen_path, 8760 * 4); // 15 min data

    int status = run_module(data, "utilityrate5");
    EXPECT_FALSE(status);

    ssc_number_t cost_without_system;
    ssc_data_get_number(data, "elec_cost_without_system_year1", &cost_without_system);
    EXPECT_NEAR(771.8, cost_without_system, 0.1); // Same as hourly, good!

    ssc_number_t cost_with_system;
    ssc_data_get_number(data, "elec_cost_with_system_year1", &cost_with_system);
    EXPECT_NEAR(-157.0, cost_with_system, 0.1);
}

TEST(cmod_utilityrate5_eqns, Test_Residential_TOU_Rates_no_credit) {
    ssc_data_t data = new var_table;

    setup_residential_rates(data);

    int analysis_period = 25;
    ssc_data_set_number(data, "ur_nm_yearend_sell_rate", 0.0);
    ssc_data_set_number(data, "system_use_lifetime_output", 1);
    ssc_data_set_number(data, "analysis_period", analysis_period);
    set_array(data, "load", load_profile_path, 8760);
    set_array(data, "gen", gen_path, 8760 * analysis_period);

    int status = run_module(data, "utilityrate5");
    EXPECT_FALSE(status);

    ssc_number_t cost_without_system;
    ssc_data_get_number(data, "elec_cost_without_system_year1", &cost_without_system);
    EXPECT_NEAR(771.8, cost_without_system, 0.1);

    ssc_number_t cost_with_system;
    ssc_data_get_number(data, "elec_cost_with_system_year1", &cost_with_system);
    EXPECT_NEAR(64.03, cost_with_system, 0.1);
}

TEST(cmod_utilityrate5_eqns, Test_Residential_TOU_Rates_annual_minimum) {
    ssc_data_t data = new var_table;

    setup_residential_rates(data);

    int analysis_period = 25;
    ssc_data_set_number(data, "ur_nm_yearend_sell_rate", 0.0);
    ssc_data_set_number(data, "ur_annual_min_charge", 100);
    ssc_data_set_number(data, "system_use_lifetime_output", 1);
    ssc_data_set_number(data, "analysis_period", analysis_period);
    set_array(data, "load", load_profile_path, 8760);
    set_array(data, "gen", gen_path, 8760 * analysis_period);

    int status = run_module(data, "utilityrate5");
    EXPECT_FALSE(status);

    ssc_number_t cost_without_system;
    ssc_data_get_number(data, "elec_cost_without_system_year1", &cost_without_system);
    EXPECT_NEAR(771.8, cost_without_system, 0.1);

    ssc_number_t cost_with_system;
    ssc_data_get_number(data, "elec_cost_with_system_year1", &cost_with_system);
    EXPECT_NEAR(100.0, cost_with_system, 0.1);
}
TEST(cmod_utilityrate5_eqns, Test_Residential_TOU_Rates_w_tiers) {
    ssc_data_t data = new var_table;

    ssc_data_set_number(data, "en_electricity_rates", 1);
    ssc_data_set_number(data, "ur_en_ts_sell_rate", 0);
    ssc_number_t p_ur_ts_buy_rate[1] = { 0 };
    ssc_data_set_array(data, "ur_ts_buy_rate", p_ur_ts_buy_rate, 1);
    ssc_number_t p_ur_ec_sched_weekday[288] = { 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 6, 6, 6, 6, 5, 5, 4, 4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 6, 6, 6, 6, 5, 5, 4, 4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4, 4, 5, 5, 5, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4, 4, 5, 5, 5 };
    ssc_data_set_matrix(data, "ur_ec_sched_weekday", p_ur_ec_sched_weekday, 12, 24);
    ssc_number_t p_ur_ec_sched_weekend[288] = { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 5, 4, 4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 5, 4, 4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 5, 4, 4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 5, 4, 4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 5, 4, 4, 4, 4, 4, 5, 5, 5, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 5, 4, 4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 5, 4, 4, 4, 4, 4, 5, 5, 5 };
    ssc_data_set_matrix(data, "ur_ec_sched_weekend", p_ur_ec_sched_weekend, 12, 24);
    ssc_number_t p_ur_ec_tou_mat[72] = { 1, 1, 11.699999999999999, 2, 0.31868000000000002, 0, 1, 2, 9.9999999999999998e+37, 2, 0.50883999999999996, 0, 2, 1, 11.699999999999999, 2, 0.26424999999999998, 0, 2, 2, 9.9999999999999998e+37, 2, 0.45440999999999998, 0, 3, 1, 11.699999999999999, 2, 0.21035000000000001, 0, 3, 2, 9.9999999999999998e+37, 2, 0.40050999999999998, 0, 4, 1, 11.960000000000001, 2, 0.23241000000000001, 0, 4, 2, 9.9999999999999998e+37, 2, 0.40853, 0, 5, 1, 11.960000000000001, 2, 0.22394, 0, 5, 2, 9.9999999999999998e+37, 2, 0.40006000000000003, 0, 6, 1, 11.960000000000001, 2, 0.21454000000000001, 0, 6, 2, 9.9999999999999998e+37, 2, 0.39066000000000001, 0 };
    ssc_data_set_matrix(data, "ur_ec_tou_mat", p_ur_ec_tou_mat, 12, 6);
    ssc_number_t p_ppa_price_input[1] = { 0.089999999999999997 };
    ssc_data_set_array(data, "ppa_price_input", p_ppa_price_input, 1);
    ssc_data_set_number(data, "ppa_multiplier_model", 1);
    ssc_data_set_number(data, "inflation_rate", 2.5);
    ssc_number_t p_degradation[1] = { 0 };
    ssc_data_set_array(data, "degradation", p_degradation, 1);
    ssc_number_t p_rate_escalation[1] = { 0 };
    ssc_data_set_array(data, "rate_escalation", p_rate_escalation, 1);
    ssc_data_set_number(data, "ur_metering_option", 0);
    ssc_data_set_number(data, "ur_nm_yearend_sell_rate", 0.027890000000000002);
    ssc_data_set_number(data, "ur_monthly_fixed_charge", 0);
    ssc_data_set_number(data, "ur_monthly_min_charge", 0);
    ssc_data_set_number(data, "ur_annual_min_charge", 0);
    ssc_number_t  ur_ts_sell_rate[1] = { 0 };
    ssc_data_set_array(data, "ur_ts_sell_rate", ur_ts_sell_rate, 1);
    ssc_data_set_number(data, "ur_dc_enable", 0);
    ssc_number_t p_ur_dc_sched_weekday[288] = { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 };
    ssc_data_set_matrix(data, "ur_dc_sched_weekday", p_ur_dc_sched_weekday, 12, 24);
    ssc_number_t p_ur_dc_sched_weekend[288] = { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 };
    ssc_data_set_matrix(data, "ur_dc_sched_weekend", p_ur_dc_sched_weekend, 12, 24);
    ssc_number_t p_ur_dc_tou_mat[8] = { 1, 1, 9.9999999999999998e+37, 0, 2, 1, 9.9999999999999998e+37, 0 };
    ssc_data_set_matrix(data, "ur_dc_tou_mat", p_ur_dc_tou_mat, 2, 4);
    ssc_number_t p_ur_dc_flat_mat[48] = { 0, 1, 9.9999999999999998e+37, 0, 1, 1, 9.9999999999999998e+37, 0, 2, 1, 9.9999999999999998e+37, 0, 3, 1, 9.9999999999999998e+37, 0, 4, 1, 9.9999999999999998e+37, 0, 5, 1, 9.9999999999999998e+37, 0, 6, 1, 9.9999999999999998e+37, 0, 7, 1, 9.9999999999999998e+37, 0, 8, 1, 9.9999999999999998e+37, 0, 9, 1, 9.9999999999999998e+37, 0, 10, 1, 9.9999999999999998e+37, 0, 11, 1, 9.9999999999999998e+37, 0 };
    ssc_data_set_matrix(data, "ur_dc_flat_mat", p_ur_dc_flat_mat, 12, 4);

    int analysis_period = 25;
    ssc_data_set_number(data, "system_use_lifetime_output", 1);
    ssc_data_set_number(data, "analysis_period", analysis_period);
    set_array(data, "load", load_profile_path, 8760);
    set_array(data, "gen", gen_path, 8760 * analysis_period);

    int status = run_module(data, "utilityrate5");
    EXPECT_FALSE(status);

    ssc_number_t cost_without_system;
    ssc_data_get_number(data, "elec_cost_without_system_year1", &cost_without_system);
    EXPECT_NEAR(1839.7, cost_without_system, 0.1);

    ssc_number_t cost_with_system;
    ssc_data_get_number(data, "elec_cost_with_system_year1", &cost_with_system);
    EXPECT_NEAR(286.2, cost_with_system, 0.1);
}

TEST(cmod_utilityrate5_eqns, Test_Commercial_Demand_Charges) {
    ssc_data_t data = new var_table;

    ssc_data_set_number(data, "en_electricity_rates", 1);
    ssc_data_set_number(data, "ur_en_ts_sell_rate", 0);
    ssc_number_t p_ur_ts_buy_rate[1] = { 0 };
    ssc_data_set_array(data, "ur_ts_buy_rate", p_ur_ts_buy_rate, 1);
    ssc_number_t p_ur_ec_sched_weekday[288] = { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 4, 4, 4, 4, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 4, 4, 4, 4 };
    ssc_data_set_matrix(data, "ur_ec_sched_weekday", p_ur_ec_sched_weekday, 12, 24);
    ssc_number_t p_ur_ec_sched_weekend[288] = { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4 };
    ssc_data_set_matrix(data, "ur_ec_sched_weekend", p_ur_ec_sched_weekend, 12, 24);
    ssc_number_t p_ur_ec_tou_mat[24] = { 1, 1, 9.9999999999999998e+37, 0, 0.050000000000000003, 0, 2, 1, 9.9999999999999998e+37, 0, 0.074999999999999997, 0, 3, 1, 9.9999999999999998e+37, 0, 0.059999999999999998, 0, 4, 1, 9.9999999999999998e+37, 0, 0.050000000000000003, 0 };
    ssc_data_set_matrix(data, "ur_ec_tou_mat", p_ur_ec_tou_mat, 4, 6);
    ssc_data_set_number(data, "inflation_rate", 2.5);
    ssc_number_t p_degradation[1] = { 0 };
    ssc_data_set_array(data, "degradation", p_degradation, 1);
    ssc_number_t p_load_escalation[1] = { 0 };
    ssc_data_set_array(data, "load_escalation", p_load_escalation, 1);
    ssc_number_t p_rate_escalation[1] = { 0 };
    ssc_data_set_array(data, "rate_escalation", p_rate_escalation, 1);
    ssc_data_set_number(data, "ur_metering_option", 0);
    ssc_data_set_number(data, "ur_nm_yearend_sell_rate", 0);
    ssc_data_set_number(data, "ur_monthly_fixed_charge", 30);
    ssc_data_set_number(data, "ur_monthly_min_charge", 0);
    ssc_data_set_number(data, "ur_annual_min_charge", 0);
    ssc_number_t  ur_ts_sell_rate[1] = { 0 };
    ssc_data_set_array(data, "ur_ts_sell_rate", ur_ts_sell_rate, 1);
    ssc_data_set_number(data, "ur_dc_enable", 1);
    ssc_number_t p_ur_dc_sched_weekday[288] = { 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2 };
    ssc_data_set_matrix(data, "ur_dc_sched_weekday", p_ur_dc_sched_weekday, 12, 24);
    ssc_number_t p_ur_dc_sched_weekend[288] = { 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 };
    ssc_data_set_matrix(data, "ur_dc_sched_weekend", p_ur_dc_sched_weekend, 12, 24);
    ssc_number_t p_ur_dc_tou_mat[16] = { 1, 1, 100, 20, 1, 2, 9.9999999999999998e+37, 15, 2, 1, 100, 10, 2, 2, 9.9999999999999998e+37, 5 };
    ssc_data_set_matrix(data, "ur_dc_tou_mat", p_ur_dc_tou_mat, 4, 4);
    ssc_number_t p_ur_dc_flat_mat[48] = { 0, 1, 9.9999999999999998e+37, 0, 1, 1, 9.9999999999999998e+37, 0, 2, 1, 9.9999999999999998e+37, 0, 3, 1, 9.9999999999999998e+37, 0, 4, 1, 9.9999999999999998e+37, 0, 5, 1, 9.9999999999999998e+37, 0, 6, 1, 9.9999999999999998e+37, 0, 7, 1, 9.9999999999999998e+37, 0, 8, 1, 9.9999999999999998e+37, 0, 9, 1, 9.9999999999999998e+37, 0, 10, 1, 9.9999999999999998e+37, 0, 11, 1, 9.9999999999999998e+37, 0 };
    ssc_data_set_matrix(data, "ur_dc_flat_mat", p_ur_dc_flat_mat, 12, 4);

    int analysis_period = 25;
    ssc_data_set_number(data, "system_use_lifetime_output", 1);
    ssc_data_set_number(data, "analysis_period", analysis_period);
    set_array(data, "load", load_commercial, 8760);
    set_array(data, "gen", commercial_gen_path, 8760 * analysis_period);

    int status = run_module(data, "utilityrate5");
    EXPECT_FALSE(status);

    ssc_number_t cost_without_system;
    ssc_data_get_number(data, "elec_cost_without_system_year1", &cost_without_system);
    EXPECT_NEAR(104614.5, cost_without_system, 0.1);

    ssc_number_t cost_with_system;
    ssc_data_get_number(data, "elec_cost_with_system_year1", &cost_with_system);
    EXPECT_NEAR(92538.1, cost_with_system, 0.1);
}
