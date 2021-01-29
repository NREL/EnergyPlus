#ifndef _CMOD_SWH_TEST_H_
#define _CMOD_SWH_TEST_H_

#include <gtest/gtest.h>
#include "core.h"
#include "sscapi.h"
#include "vartab.h"
#include "../ssc/common.h"
#include "../input_cases/swh_common.h"
#include "../input_cases/code_generator_utilities.h"

class CM_SWH : public ::testing::Test {

public:
	ssc_data_t data;
	ssc_number_t calculated_value;
	ssc_number_t * calculated_array;

	void SetUp() {
		data = ssc_data_create();
		swh_common(data);
	}

	void TearDown() {
		if (data) {
			ssc_data_free(data);
			data = nullptr;
		}
		
	}

};

#endif