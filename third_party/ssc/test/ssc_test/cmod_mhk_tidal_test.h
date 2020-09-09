#ifndef CMOD_MHK_TIDAL_INPUTS_H_
#define CMOD_MHK_TIDAL_INPUTS_H_

#include <gtest/gtest.h>
#include "../test/input_cases/mhk/mhk_tidal_inputs.h"

#include "core.h"
#include "sscapi.h"

#include "vartab.h"
#include "../ssc/common.h"
#include "../test/input_cases/code_generator_utilities.h"

class CM_MHKTidal : public ::testing::Test {
private:
public:
	ssc_data_t data;
	ssc_number_t calculated_value;
	ssc_number_t * calculated_array;

	void SetUp() {
		data = ssc_data_create();
		tidal_inputs(data);
	}

	void TearDown() {
		if (data)
			ssc_data_clear(data);
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

};
#endif // !CMOD_MHK_TIDAL_INPUTS_H_

