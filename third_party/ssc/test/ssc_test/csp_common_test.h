#ifndef _CSP_COMMON_TEST_H_
#define _CSP_COMMON_TEST_H_

#include <gtest/gtest.h>
#include<vector>
#include "../ssc/common.h"
//#include "csp_financial_defaults.h"
#include "../input_cases/code_generator_utilities.h"

#define EXPECT_NEAR_FRAC(val1, val2, frac_error) EXPECT_NEAR(val1, val2, val2 * frac_error)
#define ASSERT_NEAR_FRAC(val1, val2, frac_error) ASSERT_NEAR(val1, val2, val2 * frac_error)

const double kErrorToleranceLo = 0.001;    // 0.1%
const double kErrorToleranceHi = 0.01;     // 1.0%

class CmodUnderTest {
public:
    CmodUnderTest(std::string module_name, ssc_data_t defaults)
        : module_name_{ module_name }, data_{ defaults } {}
    ~CmodUnderTest() {
        if (data_) {
            ssc_data_free(data_);
            data_ = nullptr;
        }
    }
    int RunModule() {
        int errors = run_module(this->data_, this->module_name_);
        return errors;
    }
    void SetInput(std::string name, ssc_number_t value) {
        ssc_data_set_number(this->data_, name.c_str(), value);
    }
    void SetInput(std::string name, std::string value) {
        ssc_data_set_string(this->data_, name.c_str(), value.c_str());
    }
    void SetInput(std::string name, ssc_number_t values[], int size) {
        // deprecated, replaced by the following that uses initializer_list
        ssc_data_set_array(this->data_, name.c_str(), values, size);
    }
    void SetInput(std::string name, const std::initializer_list<ssc_number_t>& values) {
        ssc_data_set_array(this->data_, name.c_str(), (ssc_number_t*)(values.begin()), values.size());
    }
    ssc_number_t GetOutput(std::string name) const {
        ssc_number_t output;
        ssc_data_get_number(this->data_, name.c_str(), &output);
        return output;
    }
private:
    const std::string module_name_;
    ssc_data_t data_;
};


ssc_data_t singleowner_defaults();

#endif
