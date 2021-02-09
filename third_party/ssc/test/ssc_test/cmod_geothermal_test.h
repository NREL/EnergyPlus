#ifndef _CMOD_GENERAL_TEST_H_
#define _CMOD_GENERAL_TEST_H_

#include <gtest/gtest.h>
#include "core.h"
#include "sscapi.h"
#include "vartab.h"
#include "../ssc/common.h"
#include "../input_cases/geothermal_common_data.h"
#include "../input_cases/code_generator_utilities.h"



class CMGeothermal : public ::testing::Test {

public: 
	ssc_data_t data;
	ssc_number_t calculated_value;
	ssc_number_t * calculated_array;

	void SetUp() {
		data = ssc_data_create();
		geothermal_singleowner_default(data);
	}

	void TearDown() {
		if (data)
			ssc_data_clear(data);
	}
};


#endif
