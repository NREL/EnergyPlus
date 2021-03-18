#ifndef _CMOD_BIOMASS_TEST_H_
#define _CMOD_BIOMASS_TEST_H_

#include <gtest/gtest.h>
#include "core.h"
#include "sscapi.h"
#include "vartab.h"
#include "../ssc/common.h"
#include "../input_cases/biomass_common.h"
#include "../input_cases/code_generator_utilities.h"

class CMBiomass : public ::testing::Test {

public:
	ssc_data_t data;
	ssc_number_t calculated_value;
	ssc_number_t * calculated_array;

	void SetUp() {
		data = ssc_data_create();
		biomass_commondata(data);
	}

	void TearDown() {
		if (data) {
			ssc_data_free(data);
			data = nullptr;
		}
	}

};

#endif