#ifndef SYSTEM_ADVISOR_MODEL_LIB_RESILIENCE_TEST_H
#define SYSTEM_ADVISOR_MODEL_LIB_RESILIENCE_TEST_H

#include <gtest/gtest.h>
#include <lib_battery_dispatch.h>
#include <lib_power_electronics.h>
#include <lib_shared_inverter.h>

#include "cmod_battery.h"
#include "cmod_battwatts.h"
#include "lib_resilience.h"
#include "lib_battery.h"

class ResilienceTest_lib_resilience : public ::testing::Test {
protected:
    int chem;
    int pos;
    int dispatch_mode;
    double size_kw;
    double size_kwh;
    double inv_eff;
    std::vector<double> ac;
    std::vector<double> load;
    std::vector<double> dispatch_custom;


    std::shared_ptr<batt_variables> batt_vars = nullptr;
    var_table* vartab = nullptr;
    std::shared_ptr<battstor> batt = nullptr;
    dispatch_t* dispatch = nullptr;
    SharedInverter* inverter = nullptr;

    class fakeInverter : public SharedInverter{
    public:
        fakeInverter():SharedInverter(NONE, 1, nullptr, nullptr, nullptr){
            efficiencyAC = 96;
            powerAC_kW = 0.;
        }
    };

    void CreateBattery(bool ac_not_dc_connected, size_t steps_per_hour, double pv_ac, double load_ac, double batt_dc) {
        delete vartab;
        ac.clear();
        load.clear();
        dispatch_custom.clear();
        chem = battery_params::LITHIUM_ION;
        pos = dispatch_t::BEHIND;
        dispatch_mode = 2;
        size_kw = 4.0;
        size_kwh = 16.0;
        inv_eff = 96.0;
        double dt_hr = 1. / (double)steps_per_hour;
        for (size_t i = 0; i < 8760 * steps_per_hour; i++){
            ac.push_back(pv_ac);
            load.push_back(load_ac);
            dispatch_custom.push_back(batt_dc);
        }
        size_t n_recs = 8760 * steps_per_hour;
        batt_vars = battwatts_create(n_recs, 1, chem, pos, size_kwh, size_kw, inv_eff, dispatch_mode, dispatch_custom);
        if (ac_not_dc_connected)
            batt_vars->batt_topology = ChargeController::AC_CONNECTED;
        else{
            batt_vars->batt_topology = ChargeController::DC_CONNECTED;
            inverter = new fakeInverter;
        }

        vartab = new var_table;
        batt = std::make_shared<battstor>(*vartab, true, n_recs, dt_hr, batt_vars);
        batt->initialize_automated_dispatch(ac, load);
        batt->setSharedInverter(inverter);
        dispatch = batt->dispatch_model;
    }

    void TearDown() override {
        delete vartab;
    }
};


#endif //SYSTEM_ADVISOR_MODEL_LIB_RESILIENCE_TEST_H
