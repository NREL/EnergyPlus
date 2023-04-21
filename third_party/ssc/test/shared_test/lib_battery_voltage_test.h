#ifndef SAM_SIMULATION_CORE_LIB_BATTERY_VOLTAGE_TEST_H
#define SAM_SIMULATION_CORE_LIB_BATTERY_VOLTAGE_TEST_H

#include <gtest/gtest.h>

#include "lib_util.h"
//#include "lib_battery_voltage.h"
#include "lib_battery.h"

class lib_battery_voltage_test : public ::testing::Test
{
protected:
//    std::unique_ptr<battery_capacity_interface> new_cap;
    std::unique_ptr<voltage_t> model;
    std::unique_ptr<capacity_t> cap;

//    std::shared_ptr<storage_time_params> time;

//    battery_capacity_params params;
    double tol = 0.01;
    double error;

    int n_cells_series = 139;
    int n_strings = 9;
    double voltage_nom = 3.6;
    double R = 0.2;

    int nyears = 1;

public:
};

class voltage_dynamic_lib_battery_voltage_test : public lib_battery_voltage_test
{
protected:
    double Vfull = 4.1;
    double Vexp = 4.05;
    double Vnom = 3.4;
    double Qfull = 2.25;
    double Qexp = 0.04;
    double Qnom = 2.0;
    double C_rate = 0.2;

    void CreateModel(double dt_hr){
        cap = std::unique_ptr<capacity_lithium_ion_t>(new capacity_lithium_ion_t(10, 50, 95, 5, dt_hr));

        model = std::unique_ptr<voltage_t>(new voltage_dynamic_t(n_cells_series, n_strings,
                                                                 voltage_nom, Vfull, Vexp, Vnom, Qfull, Qexp, Qnom,
                                                                 C_rate, R, dt_hr));
        model->set_initial_SOC(50);
    }
};

class voltage_table_lib_battery_voltage_test : public lib_battery_voltage_test
{
protected:
    double Vfull = 4.1;
    double Vexp = 4.05;
    double Vnom = 3.4;
    std::vector<double> vals;
    util::matrix_t<double> table;

    void CreateModel(double dt_hr){
        vals = std::vector<double>({0, Vfull, 1.78, Vexp,
                                    88.9, Vnom, 99, 0});
        table = util::matrix_t<double>(4, 2, &vals);

        cap = std::unique_ptr<capacity_lithium_ion_t>(new capacity_lithium_ion_t(10, 50, 95, 5, dt_hr));
        model = std::unique_ptr<voltage_t>(new voltage_table_t(n_cells_series, n_strings, voltage_nom, table, R, dt_hr));
        model->set_initial_SOC(50);
    }

    // Additional test case based on voltage table from a user. documented in SSC issue 412
    void CreateModel_SSC_412(double dt_hr) {
        std::vector<double> voltage_vals = { 0, 1.7, 4, 1.7, 5, 1.58, 60, 1.5, 85, 1.4, 90, 1.3, 93, 1.2, 95, 1, 96, 0.9 };
        util::matrix_t<double> voltage_table(9, 2, &voltage_vals);

        cap = std::unique_ptr<capacity_lithium_ion_t>(new capacity_lithium_ion_t(10, 50, 95, 5, dt_hr));
        model = std::unique_ptr<voltage_t>(new voltage_table_t(n_cells_series, n_strings, voltage_nom, voltage_table, R,
                                                               dt_hr));
        model->set_initial_SOC(50);
    }
};

class voltage_vanadium_lib_battery_voltage_test : public lib_battery_voltage_test
{
protected:
    void CreateModel(double dt_hr){
        cap = std::unique_ptr<capacity_lithium_ion_t>(new capacity_lithium_ion_t(10, 50, 95, 5, dt_hr));
        model = std::unique_ptr<voltage_t>(new voltage_vanadium_redox_t(n_cells_series, n_strings, voltage_nom, R,
                                                                        dt_hr));
        model->set_initial_SOC(50);
    }
};

#endif //SAM_SIMULATION_CORE_LIB_BATTERY_VOLTAGE_TEST_H
