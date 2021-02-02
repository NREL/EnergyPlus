#include <vector>
#include <string>
#include <gtest/gtest.h>

#include "vartab.h"

TEST(Wfreader_cmod_wfreader, Test) {
    char filepath[1024];
    int n1 = sprintf(filepath, "%s/test/input_docs/weather_30m.epw", std::getenv("SSCDIR"));

    auto mod = ssc_module_create("wfreader");
    auto data = ssc_data_create();
    ssc_data_set_string(data, "file_name", filepath);
    ssc_data_set_number(data, "header_only", 1);
    EXPECT_TRUE(ssc_module_exec(mod, data));
}
