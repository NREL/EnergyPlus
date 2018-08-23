/* Copyright (c) 2018 Big Ladder Software LLC. All rights reserved.
* See the LICENSE file for additional terms and conditions. */


// Standard
#include <iostream>

// vendor
#include "gtest/gtest.h"
#include "gmock/gmock.h"

// btwxt
#include <btwxt.h>
#include <griddeddata.h>
#include <hypercube.h>
#include <error.h>
#include "fixtures.hpp"

using namespace Btwxt;


TEST(Hypercube, constructor) {
    std::size_t ndims = 3;
    std::vector<Method> methods{Method::LINEAR, Method::CUBIC, Method::LINEAR};
    Hypercube my_hypercube(ndims, methods);
    EXPECT_EQ(my_hypercube.vertices.size(), 16u);
}

TEST_F(CubicFixture, hypercube_collect_things) {
    std::size_t ndims = 2;
    std::vector<Method> methods{Method::CUBIC, Method::LINEAR};
    Hypercube my_hypercube(ndims, methods);
    GridPoint current_grid_point(target);
    WhereInTheGridIsThisPoint the_locator(current_grid_point, test_gridded_data);

    my_hypercube.collect_things(the_locator);

    EXPECT_EQ(my_hypercube.point_floor[0], the_locator.get_floor()[0]);
    EXPECT_EQ(my_hypercube.methods[0], Method::CUBIC);
    double mu = the_locator.get_weights()[0];
    EXPECT_EQ(my_hypercube.interp_coeffs[0][0], 2 * mu * mu * mu - 3 * mu * mu + 1);
    EXPECT_EQ(my_hypercube.interp_coeffs[1][0], 1 - the_locator.get_weights()[1]);
}


TEST_F(CubicFixture, hypercube_weigh_one_vertex) {
    std::size_t ndims = 2;
    GridPoint current_grid_point(target);
    test_gridded_data.set_axis_interp_method(1, Method::CUBIC);
    WhereInTheGridIsThisPoint the_locator(current_grid_point, test_gridded_data);
    std::vector<Method> methods = the_locator.get_methods();
    Hypercube my_hypercube(ndims, methods);
    my_hypercube.collect_things(the_locator);
    std::vector< std::vector<double> > spacing_mults = my_hypercube.get_spacing_mults(test_gridded_data);

    std::vector<double> mus = the_locator.get_weights();
    double mx = mus[0];
    double my = mus[1];
    double c0x = 2*mx*mx*mx - 3*mx*mx + 1;
    double c0y = 2*my*my*my - 3*my*my + 1;
    //double c1x = -2*mx*mx*mx + 3*mx*mx;
    double c1y = -2*my*my*my + 3*my*my;
    double d0x = mx*mx*mx - 2*mx*mx + mx;
    double d0y = my*my*my - 2*my*my + my;
    double d1x = mx*mx*mx - mx*mx;
    double d1y = my*my*my - my*my;
    double s1x = 5.0/10;
    double s1y = 2.0/4;
    double s0x = 5.0/9;
    double s0y = 2.0/4;

    std::vector<int> this_vertex = {0, 0};
    double weight = my_hypercube.weigh_one_vertex(this_vertex, spacing_mults);
    double expected_result = c0x*c0y;
    expected_result += -1*c0x*d1y*s1y;
    expected_result += -1*d1x*s1x*c0y;
    expected_result += d1x*s1x*d1y*s1y;
    EXPECT_DOUBLE_EQ(weight, expected_result);

    this_vertex = {-1, 1};
    weight = my_hypercube.weigh_one_vertex(this_vertex, spacing_mults);
    expected_result = -1*d0x*s0x*c1y;
    expected_result += -1*d0x*s0x*d0y*s0y;
    EXPECT_DOUBLE_EQ(weight, expected_result);

    this_vertex = {2, 0};
    weight = my_hypercube.weigh_one_vertex(this_vertex, spacing_mults);
    expected_result = d1x*s1x*c0y;
    expected_result += -1*d1x*s1x*d1y*s1y;
    EXPECT_DOUBLE_EQ(weight, expected_result);

    this_vertex = {2, 2};
    weight = my_hypercube.weigh_one_vertex(this_vertex, spacing_mults);
    expected_result = d1x*s1x*d1y*s1y;
    EXPECT_DOUBLE_EQ(weight, expected_result);
}

TEST_F(CubicFixture, hypercube_calculations) {
    std::size_t ndims = 2;
    GridPoint current_grid_point(target);
    test_gridded_data.set_axis_interp_method(1, Method::CUBIC);
    WhereInTheGridIsThisPoint the_locator(current_grid_point, test_gridded_data);
    std::vector<Method> methods = the_locator.get_methods();
    Hypercube my_hypercube(ndims, methods);
    my_hypercube.collect_things(the_locator);

    Eigen::ArrayXd result = my_hypercube.all_the_calculations(test_gridded_data);
    EXPECT_NEAR(result[0], 4.1953, 0.0001);
    EXPECT_NEAR(result[1], 11.9271, 0.0001);
}

TEST_F(OneDFixture, hypercube_calculations) {
    std::size_t ndims = 1;
    GridPoint current_grid_point(target);
    test_gridded_data.set_axis_interp_method(0, Method::CUBIC);
    WhereInTheGridIsThisPoint the_locator(current_grid_point, test_gridded_data);
    std::vector<Method> methods = the_locator.get_methods();
    Hypercube my_hypercube(ndims, methods);
    my_hypercube.collect_things(the_locator);

    Eigen::ArrayXd result = my_hypercube.all_the_calculations(test_gridded_data);
    EXPECT_NEAR(result[0], 4.804398, 0.00001);
}
