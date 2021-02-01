#ifndef _CMOD_BATTERY_PVSAMV1_TEST_H_
#define _CMOD_BATTERY_PVSAMV1_TEST_H_

#include <gtest/gtest.h>
#include "core.h"

#include "vartab.h"
#include "../ssc/common.h"
#include "../input_cases/pvsamv1_common_data.h"

struct daily_battery_stats {
	daily_battery_stats(std::vector<ssc_number_t> batt_power_data, size_t steps_per_hr=1) {
	    steps_per_hour = steps_per_hr;
	    peakKwDischarge = 0;
	    peakCycles = 0;
	    peakKwCharge = 0;
	    avgCycles = 0;
	    compute(std::move(batt_power_data));
	}
	void compute(std::vector<ssc_number_t> batt_power_data);

	size_t steps_per_hour;
	ssc_number_t peakKwCharge;
	ssc_number_t peakKwDischarge;
	ssc_number_t peakCycles;
	ssc_number_t avgCycles;
};

/**
 * Test the battery dispatch controllers when integrated with various PV systems
 */
class CMPvsamv1BatteryIntegration_cmod_pvsamv1 : public ::testing::Test {

public:

	ssc_data_t data;
	ssc_number_t calculated_value;
	ssc_number_t* calculated_array;
	double m_error_tolerance_hi = 100;
	double m_error_tolerance_lo = 0.1;

	void SetUp()
	{
		data = ssc_data_create();
		pvsamv_nofinancial_default(data);
		calculated_array = new ssc_number_t[8760];
	}
	void TearDown() {
		if (data) {
			ssc_data_free(data);
			data = nullptr;
		}
		if (calculated_array) {
			delete[] calculated_array;
		}
	}
	void SetCalculated(std::string name)
	{
		ssc_data_get_number(data, const_cast<char*>(name.c_str()), &calculated_value);
	}
	void SetCalculatedArray(std::string name)
	{
		int n;
		calculated_array = ssc_data_get_array(data, const_cast<char*>(name.c_str()), &n);
	}
};

#endif // !_CMOD_BATTERY_PVSAMV1_TEST_H_
