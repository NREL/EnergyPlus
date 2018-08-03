/* Copyright (c) 2018 Big Ladder Software LLC. All rights reserved.
* See the LICENSE file for additional terms and conditions. */


#ifndef TEST_FIXTURE_HPP_
#define TEST_FIXTURE_HPP_


#include "gtest/gtest.h"


// btwxt
#include <btwxt.h>
#include <griddeddata.h>
#include <error.h>

#define EXPECT_STDOUT(action, ExpectedOut) {  \
    std::stringstream buffer;     \
    std::streambuf *sbuf = std::cout.rdbuf();   \
    std::cout.rdbuf(buffer.rdbuf());   \
    action   \
    std::string capture = buffer.str();   \
    std::cout.rdbuf(sbuf);   \
    EXPECT_STREQ(ExpectedOut.c_str(), buffer.str().c_str());   \
}

using namespace Btwxt;


class OneDFixture : public testing::Test {
protected:
    RegularGridInterpolator test_rgi;
    GriddedData test_gridded_data;
    std::vector<double> target;

    OneDFixture() {
        std::vector<std::vector<double> > grid = {{0, 2, 5, 10}};
        std::vector<std::vector<double> > values = {{6, 5, 4, 3}};

        target = {2.5};
        test_gridded_data = GriddedData(grid, values);
        test_rgi = RegularGridInterpolator(test_gridded_data);
    }
};

class TwoDFixture : public testing::Test {
protected:
    RegularGridInterpolator test_rgi;
    GriddedData test_gridded_data;
    std::vector< std::vector<double> > values;
    std::vector<double> target;

    TwoDFixture() {
        std::vector<std::vector<double> > grid = {{0, 10, 15},
                                                  {4, 6}};
        values = {{6,  3, 2, 8,  4, 2},
                 {12, 6, 4, 16, 8, 4}};
        target = {12, 5};
        test_gridded_data = GriddedData(grid, values);
        test_gridded_data.set_axis_extrap_method(0, Method::LINEAR);
        test_rgi = RegularGridInterpolator(test_gridded_data);
    }
};

class CubicFixture : public testing::Test {
protected:
    RegularGridInterpolator test_rgi;
    GriddedData test_gridded_data;
    std::vector<double> target;

    CubicFixture() {
        std::vector<std::vector<double> > grid = {{6, 10, 15, 20},
                                                  {2, 4,  6,  8}};
        std::vector<std::vector<double> > values =
                {{4,  3,  1.5, 1, 5,  4,  2, 1, 8,  6,  3, 2, 10, 8,  4,  2},
                 {12, 10, 4,   4, 16, 12, 6, 4, 20, 16, 8, 4, 25, 20, 10, 5}};

        target = {12, 4.5};
        test_gridded_data = GriddedData(grid, values);
        test_gridded_data.set_axis_interp_method(0, Method::CUBIC);
//        test_gridded_data.set_axis_interp_method(1, Method::CUBIC);
        test_rgi = RegularGridInterpolator(test_gridded_data);
    }
};

#endif /* TEST_FIXTURE_HPP_ */
