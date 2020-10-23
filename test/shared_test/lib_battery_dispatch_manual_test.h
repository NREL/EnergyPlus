#ifndef SAM_SIMULATION_CORE_LIB_BATTERY_DISPATCH_MANUAL_TEST_H
#define SAM_SIMULATION_CORE_LIB_BATTERY_DISPATCH_MANUAL_TEST_H

#include <gtest/gtest.h>
#include <lib_battery_dispatch_manual.h>
#include "lib_battery_dispatch_test.h"


/**
* \class BatteryDispatchTest
*
* Test Manual battery dispatch algorithm
*
*/
class ManualTest_lib_battery_dispatch : public BatteryProperties , public DispatchProperties
{
protected:

    capacity_lithium_ion_t * capacityModel;
    voltage_dynamic_t * voltageModel;
    thermal_t * thermalModel;
    lifetime_t * lifetimeModel;
    losses_t * lossModel;
    battery_t * batteryModel;
    BatteryPower * batteryPower;

    dispatch_manual_t * dispatchManual{nullptr};

    double surface_area = 1.2 * 1.2 * 6;
    int n_series = 139;
    int n_strings = 89;

    double currentChargeMax = 100;
    double currentDischargeMax = 100;
    double powerChargeMax = 50;
    double powerDischargeMax = 50;

    /*! Variables to store forecast data */
    std::vector<double> pv_prediction;
    std::vector<double> load_prediction;
    std::vector<double> cliploss_prediction;

    double dtHour = 1;

public:

    void SetUp()
    {
        // For Manual Dispatch Test
        BatteryProperties::SetUp();
        q = 1000. / 89.;

        capacityModel = new capacity_lithium_ion_t(q * n_strings, SOC_init, SOC_max, SOC_min, 1.0);
        voltageModel = new voltage_dynamic_t(n_series, n_strings, Vnom_default, Vfull, Vexp, Vnom, Qfull, Qexp, Qnom,
                                             C_rate, resistance, dtHour);
        lifetimeModel = new lifetime_t(cycleLifeMatrix, dtHour, calendar_q0, calendar_a, calendar_b, calendar_c);
        thermalModel = new thermal_t(1.0, mass, surface_area, resistance, Cp, h, capacityVsTemperature, T_room);
        lossModel = new losses_t();
        batteryModel = new battery_t(dtHour, chemistry, capacityModel, voltageModel, lifetimeModel, thermalModel, lossModel);

        int numberOfInverters = 1;
        m_sharedInverter = new SharedInverter(SharedInverter::SANDIA_INVERTER, numberOfInverters, sandia, partload, ond);
    }
    void TearDown()
    {
        BatteryProperties::TearDown();
        delete batteryModel;
        delete dispatchManual;
    }
};


#endif //SAM_SIMULATION_CORE_LIB_BATTERY_DISPATCH_MANUAL_TEST_H
