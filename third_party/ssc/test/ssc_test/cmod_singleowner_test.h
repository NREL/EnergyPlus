#ifndef SYSTEM_ADVISOR_MODEL_CMOD_SINGLEOWNER_TEST_H
#define SYSTEM_ADVISOR_MODEL_CMOD_SINGLEOWNER_TEST_H

#include <gtest/gtest.h>
#include "core.h"
#include "sscapi.h"
#include "vartab.h"
#include "../ssc/common.h"
#include "../input_cases/singleowner_common.h"
#include "../input_cases/code_generator_utilities.h"

class CMSingleOwner : public ::testing::Test {

public:
    ssc_data_t data;
    ssc_number_t calculated_value;
    ssc_number_t * calculated_array;

    void SetUp() {
        data = ssc_data_create();
        singleowner_common(data);
    }

    void TearDown() {
        if (data) {
            ssc_data_free(data);
            data = nullptr;
        }

    }

};


#endif //SYSTEM_ADVISOR_MODEL_CMOD_SINGLEOWNER_TEST_H
