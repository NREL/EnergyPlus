#include <gtest/gtest.h>

#include "core.h"
#ifndef _CMOD_PVYIELD_TEST_H_
#define _CMOD_PVYIELD_TEST_H_

#include "vartab.h"
#include "../ssc/common.h"
#include "../input_cases/pvyield_cases.h"

/**
 * Used to test PVYield data from Timo to address ssc issue 26
 */
class CMPvYieldTimo : public ::testing::Test{

public:

	ssc_data_t data;
	ssc_number_t calculated_value;
	double m_error_tolerance_hi = 1.0;
	double m_error_tolerance_lo = 0.1;

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
};

#endif // !_CMOD_PVYIELD_TEST_H_
