
#ifndef _CMOD_PVWATTSV7_TEST_H_
#define _CMOD_PVWATTSV7_TEST_H_

#include <gtest/gtest.h>
#include "../ssc/core.h"
#include "../ssc/vartab.h"
#include "../ssc/common.h"
#include "../input_cases/pvwattsv7_cases.h"

/**
* CMPVWattsV7 tests cmod_pvwattsv7 using a solar resource file. SetUp() creates default case,
* which can be modified within each individual test before running compute() and tests.
*/
class CMPvwattsV7Integration_cmod_pvwattsv7 : public ::testing::Test {
protected: //doesn't really matter if this is protected or public, but you need to declare one or the other or it will default to private which doesn't work
	ssc_data_t data;

	double error_tolerance = 1.0e-3;

	bool compute();
	void SetUp() { //if you always want to set up with the same default case, this can go in the class. otherwise it probably makes sense in the test itself.
		data = ssc_data_create();
		int errors = pvwattsv7_nofinancial_testfile(data);
		EXPECT_FALSE(errors); //make sure that the test ran ok
	}
	void TearDown()
	{
		ssc_data_free(data);
		data = nullptr;
	} 
};

//this function will be available to run the pvwattsV7 compute module from within tests
bool CMPvwattsV7Integration_cmod_pvwattsv7::compute() {
	ssc_module_t module = ssc_module_create("pvwattsv7");
	if (NULL == module)
	{
		printf("error: could not create 'pvwattsv7' module.");
		ssc_data_free(data);
		return false;
	}
	if (ssc_module_exec(module, data) == 0)
	{
		printf("error during simulation.");
		ssc_module_free(module);
		ssc_data_free(data);
		return false;
	}
	ssc_module_free(module);
	return 0;
}

#endif // _CMOD_PVWATTSV7_TEST_H_
