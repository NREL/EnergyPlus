#include <chrono>

#include "cmod_battery_stateful_test.h"

typedef std::chrono::high_resolution_clock Clock;

TEST_F(CMBatteryStatefulIntegration_cmod_battery_stateful, TestStep) {
    CreateModel(1);
    double last_idx, I, SOC, V, P, Q, I_d, I_c, P_d, P_c;

    ssc_module_exec(mod, data);
    ssc_data_get_number(data, "last_idx", &last_idx);
    ssc_data_get_number(data, "V", &V);
    ssc_data_get_number(data, "P", &P);
    ssc_data_get_number(data, "Q", &Q);
    ssc_data_get_number(data, "I", &I);
    ssc_data_get_number(data, "I_dischargeable", &I_d);
    ssc_data_get_number(data, "I_chargeable", &I_c);
    ssc_data_get_number(data, "P_dischargeable", &P_d);
    ssc_data_get_number(data, "P_chargeable", &P_c);
    ssc_data_get_number(data, "SOC", &SOC);
    EXPECT_EQ(last_idx, 1);
    EXPECT_NEAR(V, 549.18, 1e-2);
    EXPECT_NEAR(P, 0.549, 1e-2);
    EXPECT_NEAR(Q, 9.125, 1e-2);
    EXPECT_NEAR(I, 1, 1e-2);
    EXPECT_NEAR(I_d, 7.30, 1e-2);
    EXPECT_NEAR(I_c, -9.34, 1e-2);
    EXPECT_NEAR(P_d, 3.36, 1e-2);
    EXPECT_NEAR(P_c, -5.32, 1e-2);
    EXPECT_NEAR(SOC, 46.94, 1e-2);

    // make a copy
    std::string js = ssc_data_to_json(data);
    auto copy = json_to_ssc_data(js.c_str());

    ssc_module_exec(mod, data);
    ssc_data_get_number(data, "last_idx", &last_idx);
    ssc_data_get_number(data, "V", &V);
    ssc_data_get_number(data, "P", &P);
    ssc_data_get_number(data, "Q", &Q);
    ssc_data_get_number(data, "I", &I);
    ssc_data_get_number(data, "I_dischargeable", &I_d);
    ssc_data_get_number(data, "I_chargeable", &I_c);
    ssc_data_get_number(data, "P_dischargeable", &P_d);
    ssc_data_get_number(data, "P_chargeable", &P_c);
    ssc_data_get_number(data, "SOC", &SOC);
    EXPECT_EQ(last_idx, 2);
    EXPECT_NEAR(V, 546.09, 1e-2);
    EXPECT_NEAR(P, 0.546, 1e-2);
    EXPECT_NEAR(Q, 8.125, 1e-2);
    EXPECT_NEAR(I, 1, 1e-2);
    EXPECT_NEAR(I_d, 6.5, 1e-2);
    EXPECT_NEAR(I_c, -10.34, 1e-2);
    EXPECT_NEAR(P_d, 2.90, 1e-2);
    EXPECT_NEAR(P_c, -5.89, 1e-2);
    EXPECT_NEAR(SOC, 41.79, 1e-2);

    // run the copy, should end up in same place
    ssc_module_exec(mod, copy);
    ssc_data_get_number(copy, "last_idx", &last_idx);
    ssc_data_get_number(copy, "V", &V);
    ssc_data_get_number(copy, "P", &P);
    ssc_data_get_number(copy, "Q", &Q);
    ssc_data_get_number(copy, "I", &I);
    ssc_data_get_number(copy, "I_dischargeable", &I_d);
    ssc_data_get_number(copy, "I_chargeable", &I_c);
    ssc_data_get_number(copy, "P_dischargeable", &P_d);
    ssc_data_get_number(copy, "P_chargeable", &P_c);
    ssc_data_get_number(copy, "SOC", &SOC);
    EXPECT_EQ(last_idx, 2);
    EXPECT_NEAR(V, 546.09, 1e-2);
    EXPECT_NEAR(P, 0.546, 1e-2);
    EXPECT_NEAR(Q, 8.125, 1e-2);
    EXPECT_NEAR(I, 1, 1e-2);
    EXPECT_NEAR(I_d, 6.5, 1e-2);
    EXPECT_NEAR(I_c, -10.34, 1e-2);
    EXPECT_NEAR(P_d, 2.90, 1e-2);
    EXPECT_NEAR(P_c, -5.89, 1e-2);
    EXPECT_NEAR(SOC, 41.79, 1e-2);
}

TEST_F(CMBatteryStatefulIntegration_cmod_battery_stateful, SubMinute) {
    CreateModel(1. / 360);
    double last_idx, current, SOC, V, P;

    ssc_module_exec(mod, data);
    ssc_data_get_number(data, "last_idx", &last_idx);
    ssc_data_get_number(data, "I", &current);
    ssc_data_get_number(data, "P", &P);
    ssc_data_get_number(data, "V", &V);
    ssc_data_get_number(data, "SOC", &SOC);
    EXPECT_EQ(last_idx, 1);
    EXPECT_NEAR(current, 1, 1e-2);
    EXPECT_NEAR(P, 0.551, 0.02);
    EXPECT_NEAR(V, 551.65, 1e-2);
    EXPECT_NEAR(SOC, 52.07, 1e-2);

    // make a copy
    std::string js = ssc_data_to_json(data);
    auto copy = json_to_ssc_data(js.c_str());

    ssc_module_exec(mod, data);
    ssc_data_get_number(data, "last_idx", &last_idx);
    ssc_data_get_number(data, "I", &current);
    ssc_data_get_number(data, "P", &P);
    ssc_data_get_number(data, "V", &V);
    ssc_data_get_number(data, "SOC", &SOC);
    EXPECT_EQ(last_idx, 2);
    EXPECT_NEAR(current, 1, 1e-2);
    EXPECT_NEAR(P, 0.551, 1e-2);
    EXPECT_NEAR(V, 551.64, 1e-2);
    EXPECT_NEAR(SOC, 52.05, 1e-2);

    // run the copy, should end up in same place
    ssc_module_exec(mod, copy);
    ssc_data_get_number(data, "last_idx", &last_idx);
    ssc_data_get_number(data, "I", &current);
    ssc_data_get_number(data, "P", &P);
    ssc_data_get_number(data, "V", &V);
    ssc_data_get_number(data, "SOC", &SOC);
    EXPECT_EQ(last_idx, 2);
    EXPECT_NEAR(current, 1, 1e-2);
    EXPECT_NEAR(P, 0.551, 1e-2);
    EXPECT_NEAR(V, 551.64, 1e-2);
    EXPECT_NEAR(SOC, 52.05, 1e-2);
}

TEST_F(CMBatteryStatefulIntegration_cmod_battery_stateful, ReadJson) {
    CreateModel(1./60.);
    std::string js = "{\"control_mode\": 1.0, \"dt_hr\": 0.016666666666666666, \"input_power\": -1608.9696000000001, \"run_sequentially\": 1.0, \"C_rate\": 0.2, \"Qexp\": 0.04, \"Qfull\": 2.25, \"Qnom\": 2.0, \"Vexp\": 4.05, \"Vfull\": 4.1, \"Vnom\": 3.4, \"Vnom_default\": 3.6, \"life_model\": 0, \"calendar_a\": 0.003, \"calendar_b\": -7280.0, \"calendar_c\": 930.0, \"calendar_choice\": 1.0, \"calendar_matrix\": [[-3.1e+231]], \"calendar_q0\": 1.02, \"chem\": 1.0, \"cycling_matrix\": [[20.0, 0.0, 100.0], [20.0, 5000.0, 80.0], [20.0, 10000.0, 60.0], [80.0, 0.0, 100.0], [80.0, 1000.0, 80.0], [80.0, 2000.0, 60.0]], \"initial_SOC\": 50.0, \"maximum_SOC\": 90.0, \"minimum_SOC\": 10.0, \"resistance\": 0.0002, \"voltage_choice\": 0.0, \"Cp\": 1500.0, \"T_room_init\": 20.0, \"cap_vs_temp\": [[-10.0, 60.0], [0.0, 80.0], [25.0, 100.0], [40.0, 100.0]], \"h\": 7.5, \"loss_choice\": 0.0, \"mass\": 98980.0, \"monthly_charge_loss\": [0.0], \"monthly_discharge_loss\": [0.0], \"monthly_idle_loss\": [0.0], \"nominal_energy\": 10000.0, \"nominal_voltage\": 500.0, \"replacement_capacity\": 0.0, \"replacement_option\": 0.0, \"replacement_schedule\": [-1.4916681462400413e-154], \"replacement_schedule_percent\": [-1.4916681462400413e-154], \"schedule_loss\": [0.0], \"surface_area\": 2071.0, \"I\": 0.0, \"I_chargeable\": -939699.5036648216, \"I_dischargeable\": 0.0, \"P\": 0.0, \"P_chargeable\": -538310.3372051578, \"P_dischargeable\": 0.0, \"Q\": 1957.713280026735, \"Q_max\": 19577.07963826714, \"SOC\": 10.000027155224982, \"T_batt\": 54.667972372468775, \"T_room\": 16.5319492435796, \"V\": 0.0, \"heat_dissipated\": 592184.2210545398, \"indices_replaced\": [0.0], \"last_idx\": 1261.0, \"loss_kw\": 0.0, \"n_replacements\": 0.0, \"I_loss\": 0.0, \"SOC_prev\": 10.000026847325115, \"T_batt_prev\": 54.54844020525578, \"average_range\": 0.0, \"cell_current\": 0.0, \"cell_voltage\": 0.0, \"chargeChange\": 0.0, \"charge_mode\": 1.0, \"n_cycles\": 0.0, \"prev_charge\": 2.0, \"q0\": 1957.713280026735, \"q1_0\": 0.0, \"q2\": 0.0, \"q2_0\": 0.0, \"q_relative\": 97.96131821295074, \"q_relative_calendar\": 97.96131821295074, \"q_relative_cycle\": 100.0, \"q_relative_thermal\": 100.0, \"qmax_lifetime\": 19577.07963826714, \"qmax_thermal\": 19577.13280026734, \"qn\": 0.0, \"rainflow_Xlt\": 0.0, \"rainflow_Ylt\": 0.040386817870492635, \"rainflow_jlt\": 2.0, \"rainflow_peaks\": [0.0, 47.9929307855364, 10.000000000000014], \"range\": 0.0, \"day_age_of_battery\" : 3, \"dq_relative_calendar_old\" : 0.001, \"life_model\": 0.0}";

    auto copy = json_to_ssc_data(js.c_str());
    double P, V, SOC;

    mod = ssc_stateful_module_create("battery_stateful", copy);
    copy = json_to_ssc_data(js.c_str());
    EXPECT_TRUE(ssc_module_exec(mod, copy));

    ssc_data_get_number(copy, "P", &P);
    ssc_data_get_number(copy, "V", &V);
    ssc_data_get_number(copy, "SOC", &SOC);

    EXPECT_TRUE(1);     // means the variable retrievals all succeeded
}

TEST_F(CMBatteryStatefulIntegration_cmod_battery_stateful, RunCurrentControl) {
    double dt_hour = 1.0 / 360;

    CreateKokamModel(dt_hour);

    double range, avg_range, n_cycles, q_max, q_rel;

    std::vector<double> currents = getCurrentData();

    EXPECT_EQ(currents.size(), 2000);

    for (double current : currents) {
        ssc_data_set_number(data, "input_current", current);
        ssc_module_exec(mod, data);
    }

    ssc_data_get_number(data, "range", &range);
    ssc_data_get_number(data, "average_range", &avg_range);
    ssc_data_get_number(data, "n_cycles", &n_cycles);
    ssc_data_get_number(data, "Q_max", &q_max);
    ssc_data_get_number(data, "q_relative", &q_rel);

    EXPECT_NEAR(range, 75.330, 0.01);
    EXPECT_NEAR(avg_range, 61.535, 0.01);
    EXPECT_NEAR(n_cycles, 3.0, 0.01);
    EXPECT_NEAR(q_max, 75.56, 0.01);
    EXPECT_NEAR(q_rel, 100.853, 0.01);
}

TEST_F(CMBatteryStatefulIntegration_cmod_battery_stateful, AdaptiveTimestep) {
    CreateModel(1.);

    double power = 1;
    ssc_data_set_number(data, "control_mode", 1);
    ssc_data_set_number(data, "input_power", power);

    // create adaptive
    var_table data_copy;
    data_copy = *static_cast<var_table*>(data);
    ssc_data_set_number(&data_copy, "input_power", power);
    auto adaptive_batt = ssc_stateful_module_create("battery_stateful", &data_copy);

    double P, hourly_E = 0, adaptive_E = 0;

    // run both at hourly
    ssc_module_exec(mod, data);
    ssc_data_get_number(data, "P", &P);
    hourly_E += P;

    ssc_module_exec(adaptive_batt, &data_copy);
    ssc_data_get_number(&data_copy, "P", &P);
    adaptive_E += P;

    // run at hourly and adaptive at 15 min
    ssc_module_exec(mod, data);
    ssc_data_get_number(data, "P", &P);
    hourly_E += P;

    size_t steps_per_hour = 4;
    ssc_data_set_number(&data_copy, "dt_hr", 1. / (double)steps_per_hour);
    for (size_t i = 0; i < steps_per_hour; i++) {
        ssc_module_exec(adaptive_batt, &data_copy);
        ssc_data_get_number(&data_copy, "P", &P);
        adaptive_E += P / (double)steps_per_hour;
    }

    // run both at hourly
    ssc_module_exec(mod, data);
    ssc_data_get_number(data, "P", &P);
    hourly_E += P;

    ssc_data_set_number(&data_copy, "dt_hr", 1.);
    ssc_module_exec(adaptive_batt, &data_copy);
    ssc_data_get_number(&data_copy, "P", &P);
    adaptive_E += P;

    double hourly_SOC, adaptive_SOC;
    ssc_data_get_number(data, "SOC", &hourly_SOC);
    ssc_data_get_number(&data_copy, "SOC", &adaptive_SOC);

    EXPECT_NEAR(hourly_E, 2.994, 1e-3);
    EXPECT_NEAR(adaptive_E, 2.994, 1e-3);
    EXPECT_NEAR(hourly_SOC, 23.370, 1e-3);
    EXPECT_NEAR(adaptive_SOC, 23.428, 1e-3);

}
