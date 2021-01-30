#include "lib_resilience_test.h"

TEST_F(ResilienceTest_lib_resilience, VoltageCutoffParameterSetup)
{
    int n_series = 2;
    int n_strings = 3;
    for (auto dtHour : {1., .5, 0.25}){
        for (auto Vfull : {1.1, 2.2, 2.5, 3.3, 3.8, 4.4, 5.5}){
            for (auto Vexp : {.8, .85, .9}){
                Vexp *= Vfull;
                for (auto Vnom : {.8, .85, .9}){
                    Vnom *= Vexp;
                    for (auto Qfull : {2., 5.6, 17., 25., 35., 55., 70.}){
                        for (auto Qexp : {0.9975, 0.98, 0.97}){
                            Qexp *= Qfull;
                            for (auto Qnom : {0.8, 0.9}){
                                Qnom *= Qexp;
                                for (auto C_rate : {0.05, 0.1, 0.2}){
                                    for (auto resistance : {0.05, 0.1, 0.2}){
                                        char buf[300];
                                        sprintf(buf, "dtHour, %f, Vfull, %f, Vexp, %f, Vnom, %f, Qfull, %f, Qexp, %f, Qnom, %f, C rate, %f, res, %f",
                                                dtHour, Vfull, Vexp, Vnom, Qfull, Qexp, Qnom, C_rate, resistance);
                                        auto voltageModel = new voltage_dynamic_t(n_series, n_strings, Vnom * 0.98,
                                                                                  Vfull, Vexp, Vnom, Qfull, Qexp, Qnom,
                                                                                  C_rate, resistance, dtHour);
                                        try{
                                            double current1;
                                            for (auto q_ratio : {0.25, 0.5, 0.75}){
                                                double q = n_strings * Qfull * q_ratio;
                                                double qmax = n_strings * Qfull;
                                                auto max_1 = voltageModel->calculate_max_discharge_w(q, qmax, 0, &current1);
                                                auto power1 = voltageModel->calculate_voltage_for_current(current1, q - current1 * dtHour, qmax, 0) * current1;
                                                EXPECT_NEAR(max_1, power1, 1e-3) << buf << ", q_ratio, " << q_ratio;
                                            }
                                            delete voltageModel;

                                        }
                                        catch (std::exception&){
                                            std::cerr << buf;
                                            delete voltageModel;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

TEST_F(ResilienceTest_lib_resilience, DischargeBatteryModelHourly)
{
    CreateBattery(false, 1, 0. ,1., 1.);

    auto battery_model = batt->battery_model;
    battery_model->changeSOCLimits(0, 100);
    double current = 1;
    while (battery_model->SOC() > 40)
        batt->battery_model->run(0, current);

    battery_t initial_batt = battery_t(*batt->battery_model);
    auto battery = new battery_t(initial_batt);

    double current1;
    double max_power = battery_model->calculate_max_discharge_kw(&current1);

    // this is the estimated max power, but the actual limit is 3981.12
    EXPECT_NEAR(max_power, 3.947, 5);

    double desired_power = 0.;
    while (desired_power < max_power * 1.2){
        double target = desired_power;
        current = battery->calculate_current_for_power_kw(target);
        battery->run(1, current);

        double actual_power = battery->I() * battery->V() / 1000.;
        if (desired_power < 3.981 ){
            EXPECT_NEAR(actual_power, desired_power, 1e-2);
        }
        else{
            EXPECT_LT(actual_power, desired_power);
        }

        desired_power += max_power / 100.;
        delete battery;
        battery = new battery_t(initial_batt);
    }
}

TEST_F(ResilienceTest_lib_resilience, DischargeBatteryModelSubHourly)
{
    CreateBattery(false, 2, 0. ,1., 1.);
    batt->battery_model->changeSOCLimits(0, 100);
    double current = 1;
    while (batt->battery_model->SOC() > 40)
        batt->battery_model->run(0, current);

    battery_t initial_batt = battery_t(*batt->battery_model);
    auto battery = new battery_t(initial_batt);

    double max_current;
    double max_power = batt->battery_model->calculate_max_discharge_kw(&max_current);

    EXPECT_NEAR(max_power, 8.199, 5);

    double desired_power = 0.;
    while (desired_power < max_power * 1.2){
        double target = desired_power;
        current = battery->calculate_current_for_power_kw(target);
        battery->run(1, current);
        double actual_power = battery->I() * battery->V() / 1000.;

        if (desired_power < max_power){
            EXPECT_NEAR(actual_power, desired_power, 1e-2);
        }
        else{
            EXPECT_LT(actual_power, desired_power);
        }

        desired_power += max_power / 100.;
        delete battery;
        battery = new battery_t(initial_batt);
    }
}

TEST_F(ResilienceTest_lib_resilience, ChargeBatteryModelHourly)
{
    CreateBattery(false, 1, 0. ,1., 1.);

    batt->battery_model->changeSOCLimits(0, 100);

    double current = -1;
    while (batt->battery_model->SOC() > 90)
        batt->battery_model->run(0, current);

    battery_t initial_batt = battery_t(*batt->battery_model);
    auto battery = new battery_t(initial_batt);

    double max_power = batt->battery_model->calculate_max_charge_kw() * -1;

    double desired_power = 0.;
    while (desired_power < max_power * 1.2){
        double desired_power_neg = -1 * desired_power;
        current = battery->calculate_current_for_power_kw(desired_power_neg);
        battery->run(1, current);

        if (desired_power < max_power)
            EXPECT_NEAR(battery->I() * battery->V() / 1000., -desired_power, 1e-2);
        else{
            EXPECT_NEAR(battery->I() * battery->V() / 1000., -max_power, 1e-2);
        }

        desired_power += max_power / 100.;
        delete battery;
        battery = new battery_t(initial_batt);
    }
}

TEST_F(ResilienceTest_lib_resilience, ChargeBatteryModelSubhourly)
{
    CreateBattery(false, 2, 0. ,1., 1.);
    batt->battery_model->changeSOCLimits(0, 100);

    double current = -1;
    while (batt->battery_model->SOC() > 90)
        batt->battery_model->run(0, current);

    battery_t initial_batt = battery_t(*batt->battery_model);
    auto battery = new battery_t(initial_batt);

    double max_power = batt->battery_model->calculate_max_charge_kw() * -1;

    double desired_power = 0.;
    while (desired_power < max_power * 1.2){
        double desired_power_neg = -1 * desired_power;
        current = battery->calculate_current_for_power_kw(desired_power_neg);
        battery->run(1, current);

        if (desired_power < max_power)
            EXPECT_NEAR(battery->I() * battery->V() / 1000., -desired_power, 1e-2);
        else{
            EXPECT_NEAR(battery->I() * battery->V() / 1000., -max_power, 1e-2);
        }

        desired_power += max_power / 100.;
        delete battery;
        battery = new battery_t(initial_batt);
    }
}

TEST_F(ResilienceTest_lib_resilience, PVWattsSetUp)
{
    CreateBattery(false, 1, 0. ,1., 1.);

//    auto vol = batt->battery_model->voltage_model();
    batt->battery_model->changeSOCLimits(0, 100);

//    auto power_model = batt->dispatch_model->getBatteryPowerFlow()->getBatteryPower();

    size_t count = 0;
    while (count < 100.){
        batt->advance(vartab, ac[count], 500);

//        printf("%f\t current, %f\t voltage, %f\t losses, %f\t power\n",
//               cap->I(), vol->V(), batt->battery_model->losses_model()->getLoss(count), power_model->powerBatteryDC);

        count ++;
    }
}

TEST_F(ResilienceTest_lib_resilience, VoltageTable)
{
    std::vector<double> vals = {99, 0, 50, 2, 0, 3};
    util::matrix_t<double> table(3, 2, &vals);
    double soc_init = 50;
    auto volt = voltage_table_t(1, 1, 3, table, 0.1, 1);
    auto cap = capacity_lithium_ion_t(2.25, soc_init, 100, 0, 1);

    volt.updateVoltage(cap.q0(), cap.qmax(), cap.I(), 0, 0.);
    EXPECT_NEAR(cap.SOC(), 50, 1e-3);
    EXPECT_NEAR(volt.cell_voltage(), 2, 1e-3);


    double current = -2.;
    cap.updateCapacity(current, 1);
    volt.updateVoltage(cap.q0(), cap.qmax(), cap.I(), 0, 0.);
    EXPECT_NEAR(cap.SOC(), 100, 1e-3);
    EXPECT_NEAR(volt.cell_voltage(), 3, 1e-3);

    current = 4.;
    cap.updateCapacity(current, 1);
    volt.updateVoltage(cap.q0(), cap.qmax(), cap.I(), 0, 0.);
    EXPECT_NEAR(cap.SOC(), 0, 1e-3);
    EXPECT_NEAR(volt.cell_voltage(), 0, 1e-3);

    current = -1;
    cap.updateCapacity(current, 1);
    volt.updateVoltage(cap.q0(), cap.qmax(), cap.I(), 0, 0.);
    EXPECT_NEAR(cap.SOC(), 44.445, 1e-3);
    EXPECT_NEAR(volt.cell_voltage(), 1.773, 1e-3);

    current = -1;
    cap.updateCapacity(current, 1);
    volt.updateVoltage(cap.q0(), cap.qmax(), cap.I(), 0, 0.);
    EXPECT_NEAR(cap.SOC(), 88.889, 1e-3);
    EXPECT_NEAR(volt.cell_voltage(), 2.777, 1e-3);
}

TEST_F(ResilienceTest_lib_resilience, DischargeVoltageTable){
    std::vector<double> vals = {99, 0, 50, 2, 0, 3};
    util::matrix_t<double> table(3, 2, &vals);
    double soc_init = 50;
    auto volt = voltage_table_t(1, 1, 3, table, 0.1, 1);
    auto cap = capacity_lithium_ion_t(2.25, soc_init, 100, 0, 1);

    // test discharging
    double req_cur = volt.calculate_current_for_target_w(2.2386, 2.25, 2.25, 0);
    EXPECT_NEAR(req_cur, 1.11375, 1e-2);

    req_cur = volt.calculate_current_for_target_w(1.791, 2.25, 2.25, 0);
    EXPECT_NEAR(req_cur, 0.7748, 1e-2);

    req_cur = volt.calculate_current_for_target_w(1.343, 2.25, 2.25, 0);
    EXPECT_NEAR(req_cur, 0.5313, 1e-2);

    req_cur = volt.calculate_current_for_target_w(0.5, cap.q0(), cap.qmax(), 0);
    cap.updateCapacity(req_cur, 1);
    volt.updateVoltage(cap.q0(), cap.qmax(), cap.I(), 0, 1);
    double v = volt.cell_voltage();
    EXPECT_NEAR(req_cur * v, 0.5, 1e-2);

    // test max discharge
    cap = capacity_lithium_ion_t(2.25, 50, 100, 0, 1);
    double max_p = volt.calculate_max_discharge_w(cap.q0(), cap.qmax(), 0, &req_cur);
    cap.updateCapacity(req_cur, 1);
    volt.updateVoltage(cap.q0(), cap.qmax(), cap.I(), 0, 1);
    EXPECT_NEAR(max_p, cap.I() * volt.cell_voltage(), 1e-3);

    // test over max discharge
    cap = capacity_lithium_ion_t(2.25, 50, 100, 0, 1);
    req_cur *= 1.5;
    cap.updateCapacity(req_cur, 1);
    volt.updateVoltage(cap.q0(), cap.qmax(), cap.I(), 0, 1);
    EXPECT_GT(max_p, cap.I() * volt.cell_voltage()) << "resulting power should be less than max";


    double overmax_I = volt.calculate_current_for_target_w(max_p * 1.1, cap.q0(), cap.qmax(), 0);
    cap = capacity_lithium_ion_t(2.25, 50, 100, 0, 1);
    cap.updateCapacity(overmax_I, 1);
    volt.updateVoltage(cap.q0(), cap.qmax(), cap.I(), 0, 1);
    EXPECT_GT(max_p, cap.I() * volt.cell_voltage()) << "resulting power should be less than max";
}

TEST_F(ResilienceTest_lib_resilience, ChargeVoltageTable){
    std::vector<double> vals = {99, 0, 50, 2, 0, 3};
    util::matrix_t<double> table(3, 2, &vals);
    double soc_init = 50;
    auto volt = voltage_table_t(1, 1, 3, table, 0.1, 1);
    auto cap = capacity_lithium_ion_t(2.25, soc_init, 100, 0, 1);

    // test charging
    double current = 10;
    cap.updateCapacity(current, 1);
    double req_cur = volt.calculate_current_for_target_w(-1.5, cap.q0(), cap.qmax(), 0);
    cap.updateCapacity(req_cur, 1);
    volt.updateVoltage(cap.q0(), cap.qmax(), cap.I(), 0, 1);
    double v = volt.cell_voltage();
    EXPECT_NEAR(req_cur * v, -1.5, 1e-2);

    // test max charge
    double max_p = volt.calculate_max_charge_w(cap.q0(), cap.qmax(), 0, &current);
    cap.updateCapacity(current, 1);
    volt.updateVoltage(cap.q0(), cap.qmax(), cap.I(), 0, 1);
    EXPECT_NEAR(max_p, cap.I() * volt.cell_voltage(), 1e-3);

    // test over max charge
    current *= -1; // reset last charge
    cap.updateCapacity(current, 1);
    current *= -1.5;
    cap.updateCapacity(current, 1);
    volt.updateVoltage(cap.q0(), cap.qmax(), cap.I(), 0, 1);
    EXPECT_NEAR(max_p, cap.I() * volt.cell_voltage(), 1e-3);
}

TEST_F(ResilienceTest_lib_resilience, VoltageVanadium){
    double SOC_init = 30;
    auto volt = voltage_vanadium_redox_t(1, 1, 1.41, 0.001, 1);
    auto cap = capacity_lithium_ion_t(11, SOC_init, 100, 0, 1);

    volt.updateVoltage(cap.q0(), cap.qmax(), cap.I(), 33, 1);
    double v = volt.cell_voltage();

    double req_cur = volt.calculate_current_for_target_w(-5, 3.3, 11, 306.25);
    cap.updateCapacity(req_cur, 1);
    volt.updateVoltage(cap.q0(), cap.qmax(), cap.I(), 33, 1);
    v = volt.cell_voltage();
    EXPECT_NEAR(req_cur * v, -5, 1e-2);

    req_cur = volt.calculate_current_for_target_w(5, cap.q0(), cap.qmax(), 306.25);
    cap.updateCapacity(req_cur, 1);
    volt.updateVoltage(cap.q0(), cap.qmax(), cap.I(), 33, 1);
    v = volt.cell_voltage();
    EXPECT_NEAR(req_cur * v, 5, 1e-2);

    double max_p = volt.calculate_max_charge_w(cap.q0(), cap.qmax(), 306.15, &req_cur);
    cap.updateCapacity(req_cur, 1);
    volt.updateVoltage(cap.q0(), cap.qmax(), cap.I(), 33, 1);
    EXPECT_NEAR(max_p, cap.I() * volt.cell_voltage(), 1e-3);

    max_p = volt.calculate_max_discharge_w(cap.q0(), cap.qmax(), 306.15, &req_cur);
    cap.updateCapacity(req_cur, 1);
    volt.updateVoltage(cap.q0(), cap.qmax(), cap.I(), 33, 1);
    EXPECT_NEAR(max_p, cap.I() * volt.cell_voltage(), 1e-3);
}

TEST_F(ResilienceTest_lib_resilience, PVWattsACHourly_Discharge)
{
    // batt is ac-connected
    CreateBattery(true, 1, 0. ,1., 1.);

    resilience_runner resilience(batt);
    const double voltage = 500;
    std::vector<double> batt_power, charge_total;
    for (size_t i = 0; i < 10; i++){
        batt->initialize_time(0, i, 0);
        resilience.add_battery_at_outage_timestep(*dispatch, i);
        resilience.run_surviving_batteries(load[i], 0, 0, 0, 0, 0);
        batt->advance(vartab, ac[i], voltage, load[i]);
        charge_total.emplace_back(batt->battery_model->charge_total());
        if (i < 5)
            EXPECT_NEAR(batt->outBatteryPower[i], 1., 1e-3) << "timestep " << i;
        else
            EXPECT_LT(batt->outBatteryPower[i], 1.) << "timestep " << i;

    }
    std::vector<double> correct_charge_total = {13.86, 11.96, 10.05, 8.10, 6.10, 4.72, 4.72, 4.72, 4.72, 4.72};

    for (size_t i = 0; i < correct_charge_total.size(); i++){
        EXPECT_NEAR(charge_total[i], correct_charge_total[i], 0.1) << i;
    }

    resilience.run_surviving_batteries_by_looping(&load[0], &ac[0]);
    double avg_hours = resilience.compute_metrics();
    EXPECT_NEAR(avg_hours, 0.0028, 1e-4);

    auto survived_hours = resilience.get_hours_survived();
    EXPECT_EQ(survived_hours[0], 6);
    EXPECT_EQ(survived_hours[1], 5);
    EXPECT_EQ(survived_hours[2], 4);
    EXPECT_EQ(survived_hours[3], 3);
    EXPECT_EQ(survived_hours[4], 2);
    EXPECT_EQ(survived_hours[5], 1);
    EXPECT_EQ(survived_hours[9], 1);

    auto outage_durations = resilience.get_outage_duration_hrs();
    EXPECT_EQ(outage_durations[0], 0);
    EXPECT_EQ(outage_durations[1], 1);
    EXPECT_EQ(outage_durations[2], 2);
    EXPECT_EQ(outage_durations[3], 3);
    EXPECT_EQ(outage_durations[4], 4);
    EXPECT_EQ(outage_durations[5], 5);
    EXPECT_EQ(outage_durations[6], 6);

    auto probs = resilience.get_probs_of_surviving();
    EXPECT_NEAR(probs[0], 0.999, 1e-3);
    EXPECT_NEAR(probs[1], 0.000571, 1e-6);
    EXPECT_NEAR(probs[2], 0.000114, 1e-6);
    EXPECT_NEAR(probs[3], 0.000114, 1e-6);
    EXPECT_NEAR(probs[4], 0.000114, 1e-6);

    double avg_load = resilience.get_avg_crit_load_kwh();
    EXPECT_NEAR(avg_load, 0.003504, 1e-4);
}

TEST_F(ResilienceTest_lib_resilience, PVWattsACHalfHourly_Discharge)
{
    // batt is ac-connected
    CreateBattery(true, 2, 0. ,1., 1.);

    resilience_runner resilience(batt);
    const double voltage = 500;
    std::vector<double> batt_power, charge_total;
    for (size_t i = 0; i < 10; i++){
        for (size_t j = 0; j < 2; j++){
            batt->initialize_time(0, i, j);
            resilience.add_battery_at_outage_timestep(*dispatch, i * 2 + j);
            resilience.run_surviving_batteries(load[i], 0, 0, 0, 0, 0);
            batt->advance(vartab, ac[i], voltage, load[i]);
            EXPECT_NEAR(batt->outBatteryPower[i], 1., 1e-3) << "timestep " << i * 2 + j;
        }
        charge_total.emplace_back(batt->battery_model->charge_total());
    }
    std::vector<double> correct_charge_total = {13.86, 11.96, 10.05, 8.10, 6.10, 4.72, 4.72, 4.72, 4.72, 4.72};

    for (size_t i = 0; i < correct_charge_total.size(); i++){
        EXPECT_NEAR(charge_total[i], correct_charge_total[i], 0.1);
    }

    resilience.run_surviving_batteries_by_looping(&load[0], &ac[0]);
    double avg_hours = resilience.compute_metrics();
    EXPECT_NEAR(avg_hours, 0.0030, 1e-4);

    auto survived_hours = resilience.get_hours_survived();
    EXPECT_EQ(survived_hours[0], 6.5);
    EXPECT_EQ(survived_hours[1], 6);
    EXPECT_EQ(survived_hours[2], 5.5);
    EXPECT_EQ(survived_hours[3], 5);
    EXPECT_EQ(survived_hours[4], 4.5);
    EXPECT_EQ(survived_hours[5], 4);
    EXPECT_EQ(survived_hours[6], 3.5);
    EXPECT_EQ(survived_hours[7], 3);
    EXPECT_EQ(survived_hours[11], 1);

    auto outage_durations = resilience.get_outage_duration_hrs();
    EXPECT_EQ(outage_durations[0], 0);
    EXPECT_EQ(outage_durations[1], 1);
    EXPECT_EQ(outage_durations[2], 1.5);
    EXPECT_EQ(outage_durations[3], 2);
    EXPECT_EQ(outage_durations[7], 4);

    auto probs = resilience.get_probs_of_surviving();
    EXPECT_NEAR(probs[0], 0.999, 1e-3);
    EXPECT_NEAR(probs[1], 0.000514, 1e-6);
    EXPECT_NEAR(probs[2], 0.0000571, 1e-6);
    EXPECT_NEAR(probs[3], 0.0000571, 1e-6);

    double avg_load = resilience.get_avg_crit_load_kwh();
    EXPECT_NEAR(avg_load, 0.00348, 1e-4);
}


// for dc-connected battery, the dispatch array units are kwDC. This doesn't affect
// dispatch_resiliency which decides the dispatch based on the ac load, but it will affect
// batt::advance, which will be dispatching less power per time step than in the AC case
// where the dispatch kWAC was converted to kWDC
TEST_F(ResilienceTest_lib_resilience, PVWattsDCHourly_Discharge)
{
    CreateBattery(false, 1, 0. ,1., 1.);

    resilience_runner resilience(batt);
    const double voltage = 500;
    std::vector<double> batt_power, charge_total;
    for (size_t i = 0; i < 10; i++){
        batt->initialize_time(0, i, 0);
        resilience.add_battery_at_outage_timestep(*dispatch, i);
        resilience.run_surviving_batteries(load[i], 0, 0, 0, 0, 0);
        batt->advance(vartab, ac[i], voltage, load[i]);
        charge_total.emplace_back(batt->battery_model->charge_total());
        if (i < 5)
            EXPECT_NEAR(batt->outBatteryPower[i], 1. * inverter->efficiencyAC/100. * batt_vars->batt_dc_dc_bms_efficiency/100., 1e-3) << "timestep " << i << " battery discharging";
        else if (i == 5)
            EXPECT_NEAR(batt->outBatteryPower[i], 0.855, 1e-3) << "timestep 5 battery SOC limits";
        else
            EXPECT_NEAR(batt->outBatteryPower[i], 0, 1e-3) << "timestep " << i << " battery at min SOC";

    }
    std::vector<double> correct_charge_total = {13.94, 12.12, 10.28, 8.42, 6.51, 4.72, 4.72, 4.72, 4.72, 4.72};

    for (size_t i = 0; i < correct_charge_total.size(); i++){
        EXPECT_NEAR(charge_total[i], correct_charge_total[i], 0.1);
    }

    resilience.run_surviving_batteries_by_looping(&load[0], &ac[0]);
    double avg_hours = resilience.compute_metrics();
    EXPECT_NEAR(avg_hours, 0.0028, 1e-4);

    auto survived_hours = resilience.get_hours_survived();
    EXPECT_EQ(survived_hours[0], 6);
    EXPECT_EQ(survived_hours[1], 5);
    EXPECT_EQ(survived_hours[2], 4);
    EXPECT_EQ(survived_hours[3], 3);
    EXPECT_EQ(survived_hours[4], 2);
    EXPECT_EQ(survived_hours[5], 1);
    EXPECT_EQ(survived_hours[9], 1);

    auto outage_durations = resilience.get_outage_duration_hrs();
    EXPECT_EQ(outage_durations[0], 0);
    EXPECT_EQ(outage_durations[1], 1);
    EXPECT_EQ(outage_durations[2], 2);
    EXPECT_EQ(outage_durations[3], 3);
    EXPECT_EQ(outage_durations[4], 4);
    EXPECT_EQ(outage_durations[5], 5);
    EXPECT_EQ(outage_durations[6], 6);

    auto probs = resilience.get_probs_of_surviving();
    EXPECT_NEAR(probs[0], 0.999, 1e-3);
    EXPECT_NEAR(probs[1], 0.000571, 1e-6);
    EXPECT_NEAR(probs[2], 0.000114, 1e-6);
    EXPECT_NEAR(probs[3], 0.000114, 1e-6);
    EXPECT_NEAR(probs[4], 0.000114, 1e-6);

    double avg_load = resilience.get_avg_crit_load_kwh();
    EXPECT_NEAR(avg_load, 0.00352, 1e-4);
}

TEST_F(ResilienceTest_lib_resilience, PVWattsDCHalfHourly_Discharge)
{
    // batt is ac-connected
    CreateBattery(false, 2, 0. ,1., 1.);

    resilience_runner resilience(batt);
    const double voltage = 500;
    std::vector<double> batt_power, charge_total;
    for (size_t i = 0; i < 10; i++){
        for (size_t j = 0; j < 2; j++){
            batt->initialize_time(0, i, j);
            resilience.add_battery_at_outage_timestep(*dispatch, i * 2 + j);
            resilience.run_surviving_batteries(load[i], 0, 0, 0, 0, 0);
            batt->advance(vartab, ac[i], voltage, load[i]);
            EXPECT_NEAR(batt->outBatteryPower[i], 1. * inverter->efficiencyAC/100. * batt_vars->batt_dc_dc_bms_efficiency/100., 1e-3) << "timestep " << i * 2 + j;
        }
        charge_total.emplace_back(batt->battery_model->charge_total());
    }
    std::vector<double> correct_charge_total = {13.94, 12.12, 10.28, 8.42, 6.51, 4.72, 4.72, 4.72, 4.72, 4.72};

    for (size_t i = 0; i < correct_charge_total.size(); i++){
        EXPECT_NEAR(charge_total[i], correct_charge_total[i], 0.1);
    }

    resilience.run_surviving_batteries_by_looping(&load[0], &ac[0]);
    double avg_hours = resilience.compute_metrics();
    EXPECT_NEAR(avg_hours, 0.0032, 1e-4);

    auto survived_hours = resilience.get_hours_survived();
    EXPECT_EQ(survived_hours[0], 6.5);
    EXPECT_EQ(survived_hours[1], 6);
    EXPECT_EQ(survived_hours[2], 5.5);
    EXPECT_EQ(survived_hours[3], 5);
    EXPECT_EQ(survived_hours[4], 4.5);
    EXPECT_EQ(survived_hours[5], 4.5);
    EXPECT_EQ(survived_hours[6], 4);
    EXPECT_EQ(survived_hours[7], 3.5);
    EXPECT_EQ(survived_hours[11], 1.5);

    auto outage_durations = resilience.get_outage_duration_hrs();
    EXPECT_EQ(outage_durations[0], 0);
    EXPECT_EQ(outage_durations[1], 1);
    EXPECT_EQ(outage_durations[2], 1.5);
    EXPECT_EQ(outage_durations[3], 2);
    EXPECT_EQ(outage_durations[7], 4);

    auto probs = resilience.get_probs_of_surviving();
    EXPECT_NEAR(probs[0], 0.999, 1e-3);
    EXPECT_NEAR(probs[1], 0.000456, 1e-6);
    EXPECT_NEAR(probs[2], 0.0000571, 1e-6);
    EXPECT_NEAR(probs[3], 0.0000571, 1e-6);

    double avg_load = resilience.get_avg_crit_load_kwh();
    EXPECT_NEAR(avg_load, 0.00355, 1e-4);
}

TEST_F(ResilienceTest_lib_resilience, PVWattsACHourly_Charge)
{
    // batt is ac-connected
    CreateBattery(true, 1, 1. ,0.5, -0.5);

    resilience_runner resilience(batt);
    const double voltage = 500;
    std::vector<double> batt_power, charge_total;
    for (size_t i = 0; i < 5; i++){
        batt->initialize_time(0, i, 0);
        resilience.add_battery_at_outage_timestep(*dispatch, i);
        resilience.run_surviving_batteries(load[i], ac[i], 0, 0, 0, 0);
        batt->advance(vartab, ac[i], voltage, load[i]);
        charge_total.emplace_back(batt->battery_model->charge_total());
        EXPECT_NEAR(batt->outBatteryPower[i], -0.5, 0.005) << "timestep " << i;
    }
    std::vector<double> correct_charge_total = {16.61, 17.46, 18.32, 19.17, 20.02};

    for (size_t i = 0; i < correct_charge_total.size(); i++){
        EXPECT_NEAR(charge_total[i], correct_charge_total[i], 0.1);
    }

    EXPECT_EQ(resilience.get_n_surviving_batteries(), 5);

    resilience.run_surviving_batteries_by_looping(&load[0], &ac[0]);
    double avg_hours = resilience.compute_metrics();
    EXPECT_NEAR(avg_hours, 5, 1e-4);

    auto survived_hours = resilience.get_hours_survived();
    EXPECT_EQ(survived_hours[0], 8760);
    EXPECT_EQ(survived_hours[1], 8760);
    EXPECT_EQ(survived_hours[2], 8760);
    EXPECT_EQ(survived_hours[3], 8760);
    EXPECT_EQ(survived_hours[4], 8760);

    auto outage_durations = resilience.get_outage_duration_hrs();
    EXPECT_EQ(outage_durations[0], 0);
    EXPECT_EQ(outage_durations[1], 8760);

    auto probs = resilience.get_probs_of_surviving();
    EXPECT_NEAR(probs[0], 0.999, 1e-3);
    EXPECT_NEAR(probs[1], 0.000571, 1e-6);

    auto cdf = resilience.get_cdf_of_surviving();
    auto survival_fx = resilience.get_survival_function();
    for (size_t i = 0; i < cdf.size(); i++)
        EXPECT_NEAR(cdf[i] + survival_fx[i], 1., 1e-3) << i;
}
