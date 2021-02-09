#ifndef __LIB_BATTERY_DISPATCH_TEST_H__
#define __LIB_BATTERY_DISPATCH_TEST_H__

#include <gtest/gtest.h>
#include <lib_util.h>
#include <lib_battery_dispatch.h>
#include <lib_battery_powerflow.h>
#include <lib_ondinv.h>
#include <lib_power_electronics.h>
#include <lib_pvinv.h>
#include <lib_sandia.h>
#include <lib_shared_inverter.h>

#include "lib_battery_properties.h"

static size_t hour_of_year_from_index(size_t index, double dtHour) {
    return (size_t) (index * dtHour);
}

static size_t step_from_index(size_t index, double dtHour) {
    return index % (size_t) (1 / dtHour);
}

/// Structure for battery dispatch test settings
struct DispatchProperties {

    // Generic dispatch
    int dispatchChoice;
    int currentChoice;
    double minimumModeTime;
    int meterPosition;
    size_t *schedWeekday;
    util::matrix_t<size_t> scheduleWeekday;
    util::matrix_t<size_t> scheduleWeekend;
    std::vector<bool> canCharge;
    std::vector<bool> canDischarge;
    std::vector<bool> canGridcharge;
    std::map<size_t, double> percentDischarge;
    std::map<size_t, double> percentGridcharge;

    // resource
    std::vector<double> pv;
    std::vector<double> clip;

    // Front of meter auto dispatch
    std::vector<double> ppaRate;
    UtilityRate *ur{nullptr};

    sandia_inverter_t *sandia = nullptr;
    partload_inverter_t *partload = nullptr;
    ond_inverter *ond = nullptr;
    SharedInverter *m_sharedInverter = nullptr;

    /// Constructor for dispatch properties
    DispatchProperties() {

        // dispatch
        dispatchChoice = 4; // custom dispatch
        currentChoice = dispatch_t::CURRENT_CHOICE::RESTRICT_POWER;
        minimumModeTime = 0.1;
        meterPosition = dispatch_t::METERING::BEHIND;

        schedWeekday = new size_t[24 * 12];

        int i = 0;
        for (int m = 0; m < 12; m++) {
            for (int h = 0; h < 24; h++) {
                schedWeekday[i] = 1;
                if (h > 11 && h < 19) {
                    schedWeekday[i] = 3;
                }
                i++;
            }
        }
        scheduleWeekday.assign(schedWeekday, 12, 24);
        scheduleWeekend.assign(schedWeekday, 12, 24);

        for (int p = 0; p < 6; p++) {
            canCharge.push_back(1);
            canDischarge.push_back(1);
            canGridcharge.push_back(0);
            percentDischarge[p] = 100;
        }
        // dispatch FOM data for 18 steps
        ppaRate = {0.04938, 0.04938, 0.04938, 0.04938, 0.04938, 0.04938, 0.03246, 0.03246, 0.03246, 0.03246, 0.03246, // 11
                   0.03246, 0.03246, 0.03246, 0.03246, 0.03246, 0.03246, 0.15828, 0.15828, 0.15828, 0.15828, 0.15828, // 22
                   0.04938, 0.04938, 0.04938, 0.04938, 0.04938, 0.04938, 0.04938, 0.04938, 0.03246, 0.03246, 0.03246, // 33
                   0.03246, 0.03246, 0.03246, 0.03246, 0.03246, 0.03246, 0.03246, 0.03246, 0.15828, 0.15828, 0.15828, // 44
                   0.15828, 0.15828, 0.04938, 0.04938, 0.04938, 0.04938, 0.04938, 0.04938, 0.04938, 0.04938, 0.03246, // 55
                   0.03246, 0.03246, 0.03246, 0.03246, 0.03246, 0.03246, 0.03246, 0.03246, 0.03246, 0.03246, 0.15828, // 66
                   0.15828, 0.15828, 0.15828, 0.15828, 0.04938, 0.04938, 0.04938, 0.04938, 0.04938, 0.04938, 0.04938, // 77
                   0.04938, 0.03246, 0.03246, 0.03246, 0.03246, 0.03246, 0.03246, 0.03246, 0.03246, 0.03246, 0.03246, // 88
                   0.03246, 0.15828, 0.15828, 0.15828, 0.15828, 0.15828, 0.04938, 0.04938, 0.04938, 0.04938, 0.04938}; // 99

        // resource
        pv = {21603.3, 70098.2, 44484.7, 86767.2, 87052.4, 86202.2, 84205.4, 78854.6, 67702.2, 31516.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
        clip = {9767.18, 10052.4, 9202.19, 7205.42, 1854.6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };

        for (int i = 18; i < 8760; i++) {
            pv.push_back(0);
            clip.push_back(0);
        }

        // inverter

        sandia = new sandia_inverter_t();
        partload = new partload_inverter_t();
        ond = new ond_inverter();

        sandia->C0 = -2.445577e-8;
        sandia->C1 = 1.2e-5;
        sandia->C2 = 0.001461;
        sandia->C3 = -0.00151;
        sandia->Paco = 770000;
        sandia->Pdco = 791706.4375;
        sandia->Vdco = 614;
        sandia->Pso = 2859.5;
        sandia->Pntare = 0.99;
    }

    /// Destructor
    ~DispatchProperties() {
        delete schedWeekday;
        delete m_sharedInverter;
        delete sandia;
        delete partload;
        delete ond;
        delete ur;
    }
};

#endif
