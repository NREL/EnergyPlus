#include <string>
#include <gtest/gtest.h>

#include "../ssc/vartab.h"

TEST(sscapi_test, json_to_ssc_data) {
    std::string json_string = R"({"num": 5})";
    ssc_data_t dat = json_to_ssc_data(json_string.c_str());
    auto vt = static_cast<var_table *>(dat);
    EXPECT_EQ(vt->lookup("num")->num[0], 5);
    ssc_data_free(dat);

    json_string = R"({"str": "string"})";
    dat = json_to_ssc_data(json_string.c_str());
    vt = static_cast<var_table *>(dat);
    EXPECT_STRCASEEQ(vt->lookup("str")->str.c_str(), "string");
    ssc_data_free(dat);

    json_string = R"({"arr": [1, 2]})";
    dat = json_to_ssc_data(json_string.c_str());
    vt = static_cast<var_table *>(dat);
    EXPECT_EQ(vt->lookup("arr")->num[0], 1);
    EXPECT_EQ(vt->lookup("arr")->num[1], 2);
    ssc_data_free(dat);

    json_string = R"({"mat": [[1, 2], [3, 4]]})";
    dat = json_to_ssc_data(json_string.c_str());
    vt = static_cast<var_table *>(dat);
    EXPECT_EQ(vt->lookup("mat")->num[0], 1);
    EXPECT_EQ(vt->lookup("mat")->num[1], 2);
    EXPECT_EQ(vt->lookup("mat")->num[2], 3);
    EXPECT_EQ(vt->lookup("mat")->num[3], 4);
    ssc_data_free(dat);

    json_string = R"({"datarr": ["one", 2]})";
    dat = json_to_ssc_data(json_string.c_str());
    vt = static_cast<var_table *>(dat);
    EXPECT_STRCASEEQ(vt->lookup("datarr")->vec[0].str.c_str(), "one");
    EXPECT_EQ(vt->lookup("datarr")->vec[1].num[0], 2);
    ssc_data_free(dat);

    json_string = R"({"datmat": [["one", 2], [3, {"four": 4}]]})";
    dat = json_to_ssc_data(json_string.c_str());
    vt = static_cast<var_table *>(dat);
    EXPECT_STRCASEEQ(vt->lookup("datmat")->vec[0].vec[0].str.c_str(), "one");
    EXPECT_EQ(vt->lookup("datmat")->vec[0].vec[1].num[0], 2);
    EXPECT_EQ(vt->lookup("datmat")->vec[1].vec[0].num[0], 3);
    EXPECT_EQ(vt->lookup("datmat")->vec[1].vec[1].table.lookup("four")->num[0], 4);
    ssc_data_free(dat);

    json_string = R"({"table": {"entry": 1}}})";
    dat = json_to_ssc_data(json_string.c_str());
    vt = static_cast<var_table *>(dat);
    EXPECT_EQ(vt->lookup("table")->table.lookup("entry")->num[0], 1);
    ssc_data_free(dat);

    json_string = R"({"wrong": format})";
    dat = json_to_ssc_data(json_string.c_str());
    vt = static_cast<var_table *>(dat);
    EXPECT_GT(vt->lookup("error")->str.size(), 0);
}

TEST(sscapi_test, ssc_data_to_json) {
    var_table vt;
    vt.assign("num", 1);
    auto json_string = ssc_data_to_json(&vt);
    EXPECT_STRCASEEQ(json_string, "{\"num\":1.0}");
    vt.clear();
    delete json_string;

    vt.assign("str", var_data("string"));
    json_string = ssc_data_to_json(&vt);
    EXPECT_STRCASEEQ(json_string, "{\"str\":\"string\"}");
    vt.clear();
    delete json_string;

    vt.assign("arr", std::vector<double>({1, 2}));
    json_string = ssc_data_to_json(&vt);
    EXPECT_STRCASEEQ(json_string, "{\"arr\":[1.0,2.0]}");
    vt.clear();
    delete json_string;

    double vals[4] = {1, 2, 3, 4};
    vt.assign("mat", var_data(vals, 2, 2));
    json_string = ssc_data_to_json(&vt);
    EXPECT_STRCASEEQ(json_string, "{\"mat\":[[1.0,2.0],[3.0,4.0]]}");
    vt.clear();
    delete json_string;

    std::vector<var_data> vars = {var_data("one"), 2};
    vt.assign("datarr", vars);
    json_string = ssc_data_to_json(&vt);
    EXPECT_STRCASEEQ(json_string, "{\"datarr\":[\"one\",2.0]}");
    vt.clear();
    delete json_string;

    std::vector<std::vector<var_data>> vars_mat = {vars, std::vector<var_data>({3, 4})};
    vt.assign("datmat", vars_mat);
    json_string = ssc_data_to_json(&vt);
    EXPECT_STRCASEEQ(json_string, "{\"datmat\":[[\"one\",2.0],[3.0,4.0]]}");
    vt.clear();

    var_table tab;
    tab.assign("entry", 1);
    vt.assign("table", tab);
    json_string = ssc_data_to_json(&vt);
    EXPECT_STRCASEEQ(json_string, "{\"table\":{\"entry\":1.0}}");
    vt.clear();
    delete json_string;

}

