#ifndef _CMOD_GEOTHERMAL_COSTS_TEST_H_
#define _CMOD_GEOTHERMAL_COSTS_TEST_H_

#include <gtest/gtest.h>
#include <memory>

#include "core.h"
#include "sscapi.h"
#include "vartab.h"
#include "../ssc/common.h"
#include "../input_cases/code_generator_utilities.h"
#include "../input_cases/geothermal_costs_common_data.h"

/**
 * CMGeothermalCosts tests the cmod_geothermal_costs. SAM code generator cannot be used to generate data for this module since it is
 * a derived cmod of cmod_geothermal. This means that all the data required to run this cmod is a subset of data created using SAM
 * code generator. The geothermal_costs_default() function in "../input_cases/geothermal_costs_common_data.h" is a data container for the 
 * data used in this particular cmod (which was obatined using the code generator). 
 * Eventually a method can be written to write this data to a vartable so that lower-level methods of pvsamv1 can be tested
 * For now, this uses the SSCAPI interfaces to run the compute module and compare results
 */
class CMGeothermalCosts : public ::testing::Test {

public:

	ssc_data_t data;
	ssc_number_t calculated_value;
	ssc_number_t * calculated_array;

	void SetUp()
	{
		data = ssc_data_create(); //Data structure for cmod_geothermal_costs
		geothermal_costs_default(data);
	}
	void TearDown() {
		if (data) {
			ssc_data_clear(data);
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
};

#endif 
