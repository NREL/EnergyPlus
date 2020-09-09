#ifndef __LIB_BATTERY_TEST_H__
#define __LIB_BATTERY_TEST_H__

#include <gtest/gtest.h>

#include <lib_battery.h>

#include "lib_battery_properties.h"

/// Test Battery Model and submodels
class BatteryTest : public BatteryProperties
{
public:

	capacity_lithium_ion_t * capacityModel;
	voltage_dynamic_t * voltageModel;
	thermal_t * thermalModel;
	lifetime_calendar_t * calendarModel;
	lifetime_cycle_t * cycleModel;
	lifetime_t * lifetimeModel;
	losses_t * lossModel;
	battery_t * batteryModel;

	void SetUp()
	{
		BatteryProperties::SetUp();
		capacityModel = new capacity_lithium_ion_t(q, SOC_init, SOC_max, SOC_min);
		voltageModel = new voltage_dynamic_t(n_series, n_strings, Vnom_default, Vfull, Vexp, Vnom, Qfull, Qexp, Qnom,
                                             C_rate, resistance, dtHour);
		cycleModel = new lifetime_cycle_t(cycleLifeMatrix);
		calendarModel = new lifetime_calendar_t(calendarChoice, calendarLifeMatrix, dtHour);
		lifetimeModel = new lifetime_t(cycleModel, calendarModel, replacementOption, replacementCapacity);
		thermalModel = new thermal_t(1.0, mass, length, width, height, Cp, h, T_room, capacityVsTemperature);
		lossModel = new losses_t(dtHour, lifetimeModel, thermalModel, capacityModel, lossChoice, monthlyLosses, monthlyLosses, monthlyLosses, fullLosses);
		batteryModel = new battery_t(dtHour, chemistry);
		batteryModel->initialize(capacityModel, voltageModel, lifetimeModel, thermalModel, lossModel);
	}
	void TearDown() {
		if (capacityModel) {
			delete capacityModel;
			capacityModel = nullptr;
		}
		if (voltageModel) {
			delete voltageModel;
			voltageModel = nullptr;
		}
		if (cycleModel) {
			delete cycleModel;
			cycleModel = nullptr;
		}
		if (calendarModel) {
			delete calendarModel;
			calendarModel = nullptr;
		}
		if (lifetimeModel) {
			delete lifetimeModel;
			lifetimeModel = nullptr;
		}
		if (thermalModel) {
			delete thermalModel;
			thermalModel = nullptr;
		}
		if (lossModel) {
			delete lossModel;
			lossModel = nullptr;
		}
		if (batteryModel) {
			delete batteryModel;
			batteryModel = nullptr;
		}
	}
};

#endif