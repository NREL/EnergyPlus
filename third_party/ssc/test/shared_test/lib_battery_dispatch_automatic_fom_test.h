#ifndef SAM_SIMULATION_CORE_LIB_BATTERY_DISPATCH_AUTOMATIC_FOM_TEST_H
#define SAM_SIMULATION_CORE_LIB_BATTERY_DISPATCH_AUTOMATIC_FOM_TEST_H

#include <gtest/gtest.h>
#include <lib_battery_dispatch_automatic_fom.h>
#include "lib_battery_dispatch_test.h"


/**
* \class BatteryDispatchTest
*
* Automatic front of meter with AC or DC coupling of the battery dispatch algorithms
*
*/
class AutoFOM_lib_battery_dispatch : public BatteryProperties , public DispatchProperties
{
protected:
    thermal_t * thermalModel;
    lifetime_t * lifetimeModel;
    BatteryPower * batteryPower;

    capacity_lithium_ion_t * capacityModel;
    voltage_dynamic_t * voltageModel;
    losses_t * lossModel;
    battery_t *batteryModel;

    dispatch_automatic_front_of_meter_t * dispatchAuto{nullptr };

    double max_power = 25000;
    double surface_area = 686;

    /*! Variables to store forecast data */
    std::vector<double> pv_prediction;
    std::vector<double> load_prediction;
    std::vector<double> cliploss_prediction;

    std::vector<double> replacementCost = { 0.0 };
    std::vector<double> cyclingCost = { 0.005 };
public:

    void CreateBattery(double dtHour)
    {
        // For testing Automated Front-of-meter DC-coupled
        BatteryProperties::SetUp();

        capacityModel = new capacity_lithium_ion_t(2.25 * 133227, 50, 100, 10, dtHour);
        voltageModel = new voltage_dynamic_t(139, 133227, 3.6, 4.10, 4.05, 3.4,
                                             2.25, 0.04, 2.00, 0.2, 0.2, dtHour);
        lifetimeModel = new lifetime_calendar_cycle_t(cycleLifeMatrix, dtHour, calendar_q0, calendar_a, calendar_b, calendar_c);
        thermalModel = new thermal_t(1.0, mass, surface_area, resistance, Cp, h, capacityVsTemperature, T_room);
        lossModel = new losses_t();
        batteryModel = new battery_t(dtHour, chemistry, capacityModel, voltageModel, lifetimeModel, thermalModel, lossModel);

        int numberOfInverters = 40;
        m_sharedInverter = new SharedInverter(SharedInverter::SANDIA_INVERTER, numberOfInverters, sandia, partload, ond);
    }
    void CreateBatteryWithLosses(double dtHour)
    {
        // For testing Automated Front-of-meter DC-coupled
        BatteryProperties::SetUp();

        capacityModel = new capacity_lithium_ion_t(2.25 * 133227, 50, 100, 10, dtHour);
        voltageModel = new voltage_dynamic_t(139, 133227, 3.6, 4.10, 4.05, 3.4,
            2.25, 0.04, 2.00, 0.2, 0.2, dtHour);
        lifetimeModel = new lifetime_calendar_cycle_t(cycleLifeMatrix, dtHour, calendar_q0, calendar_a, calendar_b, calendar_c);
        thermalModel = new thermal_t(1.0, mass, surface_area, resistance, Cp, h, capacityVsTemperature, T_room);

        std::vector<double> charging_losses(12, 10); // Monthly losses
        std::vector<double> discharging_losses(12, 20);
        std::vector<double> idle_losses(12, 5);
        lossModel = new losses_t(charging_losses, discharging_losses, idle_losses);
        batteryModel = new battery_t(dtHour, chemistry, capacityModel, voltageModel, lifetimeModel, thermalModel, lossModel);

        int numberOfInverters = 40;
        m_sharedInverter = new SharedInverter(SharedInverter::SANDIA_INVERTER, numberOfInverters, sandia, partload, ond);
    }
    void TearDown()
    {
        BatteryProperties::TearDown();
        delete batteryModel;
        delete dispatchAuto;
    }

};



#endif //SAM_SIMULATION_CORE_LIB_BATTERY_DISPATCH_AUTOMATIC_FOM_TEST_H
