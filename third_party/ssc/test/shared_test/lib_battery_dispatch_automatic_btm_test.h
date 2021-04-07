#ifndef SAM_SIMULATION_CORE_LIB_BATTERY_DISPATCH_AUTOMATIC_BTM_TEST_H
#define SAM_SIMULATION_CORE_LIB_BATTERY_DISPATCH_AUTOMATIC_BTM_TEST_H

#include <gtest/gtest.h>
#include <lib_battery_dispatch_automatic_btm.h>
#include "lib_battery_dispatch_test.h"

/**
* \class BatteryDispatchTest
*
* Automatic behind the meter battery dispatch algorithms
*
*/
class AutoBTMTest_lib_battery_dispatch : public BatteryProperties , public DispatchProperties
{
protected:

    capacity_lithium_ion_t * capacityModel;
    voltage_dynamic_t * voltageModel;
    thermal_t * thermalModel;
    lifetime_t * lifetimeModel;
    losses_t * lossModel;
    battery_t * batteryModel;
    BatteryPower * batteryPower;

    rate_data* util_rate{ nullptr };
    dispatch_automatic_behind_the_meter_t * dispatchAutoBTM{nullptr};

    double max_power = 50;
    double max_current = 500;
    double surface_area = 1.2 * 1.2 * 6;
    int n_series = 139;
    int n_strings = 89;

    std::vector<double> replacementCost = { 0.0 };
    int cyclingChoice = 1;
    std::vector<double> cyclingCost = { 0.0 };

    /*! Variables to store forecast data */
    std::vector<double> pv_prediction;
    std::vector<double> load_prediction;
    std::vector<double> cliploss_prediction;

public:

    void CreateBattery(double dtHour)
    {
        // For Manual Dispatch Test
        BatteryProperties::SetUp();
        n_strings = 445;

        capacityModel = new capacity_lithium_ion_t(Qfull * n_strings, SOC_init, SOC_max, SOC_min, dtHour);
        voltageModel = new voltage_dynamic_t(n_series, n_strings, Vnom_default, Vfull, Vexp, Vnom, Qfull, Qexp, Qnom,
                                             C_rate, resistance, dtHour);
        lifetimeModel = new lifetime_calendar_cycle_t(cycleLifeMatrix, dtHour, calendar_q0, calendar_a, calendar_b, calendar_c);
        thermalModel = new thermal_t(1.0, mass, surface_area, resistance, Cp, h, capacityVsTemperature, T_room);
        lossModel = new losses_t();
        batteryModel = new battery_t(dtHour, chemistry, capacityModel, voltageModel, lifetimeModel, thermalModel, lossModel);

        int numberOfInverters = 40;
        m_sharedInverter = new SharedInverter(SharedInverter::SANDIA_INVERTER, numberOfInverters, sandia, partload, ond);
    }

    void CreateResidentialBattery(double dtHour)
    {
        n_strings = 9;
        CreateBattery(dtHour);
        delete m_sharedInverter;
        int numberOfInverters = 1;
        m_sharedInverter = new SharedInverter(SharedInverter::SANDIA_INVERTER, numberOfInverters, sandia, partload, ond);
    }


    void CreateBatteryWithLosses(double dtHour)
    {
        // For Manual Dispatch Test
        BatteryProperties::SetUp();
        q = 1000. / 89.;

        capacityModel = new capacity_lithium_ion_t(q * n_strings, SOC_init, SOC_max, SOC_min, dtHour);
        voltageModel = new voltage_dynamic_t(n_series, n_strings, Vnom_default, Vfull, Vexp, Vnom, Qfull, Qexp, Qnom,
            C_rate, resistance, dtHour);
        lifetimeModel = new lifetime_calendar_cycle_t(cycleLifeMatrix, dtHour, calendar_q0, calendar_a, calendar_b, calendar_c);
        thermalModel = new thermal_t(1.0, mass, surface_area, resistance, Cp, h, capacityVsTemperature, T_room);

        std::vector<double> charging_losses(12, 1); // Monthly losses
        std::vector<double> discharging_losses(12, 2);
        std::vector<double> idle_losses(12, 0.5);
        lossModel = new losses_t(charging_losses, discharging_losses, idle_losses);
        batteryModel = new battery_t(dtHour, chemistry, capacityModel, voltageModel, lifetimeModel, thermalModel, lossModel);

        int numberOfInverters = 40;
        m_sharedInverter = new SharedInverter(SharedInverter::SANDIA_INVERTER, numberOfInverters, sandia, partload, ond);
    }

    void TearDown()
    {
        BatteryProperties::TearDown();
        delete batteryModel;
        delete dispatchAutoBTM;
    }
};


#endif //SAM_SIMULATION_CORE_LIB_BATTERY_DISPATCH_AUTOMATIC_BTM_TEST_H
