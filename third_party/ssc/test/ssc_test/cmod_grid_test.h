#ifndef _CMOD_GRID_TEST_H_
#define _CMOD_GRID_TEST_H_

#include <gtest/gtest.h>
#include <memory>

#include "core.h"
#include "sscapi.h"

#include "vartab.h"
#include "../ssc/common.h"
#include "../input_cases/code_generator_utilities.h"
#include "../input_cases/grid_common_data.h"

/**
 * CMGrid tests the cmod_grid using the SAM code generator to generate data
 */
class CMGrid_cmod_grid : public ::testing::Test {

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
