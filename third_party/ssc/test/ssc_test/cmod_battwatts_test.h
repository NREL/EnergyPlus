#ifndef SAM_SIMULATION_CORE_CMOD_BATTWATTS_TEST_H
#define SAM_SIMULATION_CORE_CMOD_BATTWATTS_TEST_H

#include <gtest/gtest.h>

#include "../input_cases/sscapi.h"
#include "vartab.h"


class CMBattwatts_cmod_battwatts : public ::testing::Test {

public:

    var_table data;
    std::vector<double> ac;
    std::vector<double> load;
    std::vector<double> crit_load;

    double m_error_tolerance_hi = 1.0;
    double m_error_tolerance_lo = 0.1;

    void CreateData(size_t nyears) {
        for (size_t i = 0; i < 8760 * nyears; i++){
            size_t hr = i % 24;
            if (hr > 7 && hr < 18 )
                ac.push_back(1.);
            else
                ac.push_back(0.);
            load.push_back(0.5);
            crit_load.push_back(0.25);
        }


        data.assign("system_use_lifetime_output", nyears > 1);
        data.assign("analysis_period", (int)nyears);
        data.assign("batt_simple_enable", 1);
        data.assign("batt_simple_kwh", 10);
        data.assign("batt_simple_kw", 5);
        data.assign("batt_simple_chemistry", 1);
        data.assign("batt_simple_dispatch", 0);
        data.assign("batt_simple_meter_position", 0);
        data.assign("ac", ac);
        data.assign("load", load);
        data.assign("crit_load", crit_load);
        data.assign("inverter_model", 0);
        data.assign("inverter_efficiency", 96);
    }
};

#endif //SAM_SIMULATION_CORE_CMOD_BATTWATTS_TEST_H
