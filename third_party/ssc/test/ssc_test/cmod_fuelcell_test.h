#ifndef _CMOD_FUELCELL_TEST_H_
#define _CMOD_FUELCELL_TEST_H_

#include <gtest/gtest.h>
#include <memory>

#include "core.h"
#include "sscapi.h"

#include "vartab.h"
#include "../ssc/common.h"
#include "../input_cases/code_generator_utilities.h"
#include "../input_cases/battery_common_data.h"
#include "../input_cases/fuelcell_common_data.h"

/**
 * CMFuelCell tests the cmod_fuelcell using the SAM code generator to generate data
 * Eventually a method can be written to write this data to a vartable so that lower-level methods of pvsamv1 can be tested
 * For now, this uses the SSCAPI interfaces to run the compute module and compare results
 */
class CMFuelCell : public ::testing::Test {

public:

	ssc_data_t data;
	ssc_number_t calculated_value;
	ssc_number_t * calculated_array;
	double m_error_tolerance_hi = 1.0;
	double m_error_tolerance_lo = 0.1;
	size_t interval = 100;

	void SetUp()
	{
		data = ssc_data_create();
		fuelcell_nofinancial_default(data);
		battery_commercial_peak_shaving_lifetime(data);
	
	}
	void TearDown() {
		if (data) {
			ssc_data_free(data);
			data = nullptr;
		}
	}
	void SetCalculated(std::string name)
	{
		ssc_data_get_number(data, const_cast<char *>(name.c_str()), &calculated_value);
	}
	// apparently memory of the array is managed internally to the sscapi.
	void SetCalculatedArray(std::string name)
	{
		int n;
		calculated_array = ssc_data_get_array(data, const_cast<char *>(name.c_str()), &n);
	}
	ssc_number_t * GetArray(std::string name, int &n)
	{
		ssc_number_t * ret = ssc_data_get_array(data, const_cast<char *>(name.c_str()), &n);
		return ret;
	}


};

#endif 
