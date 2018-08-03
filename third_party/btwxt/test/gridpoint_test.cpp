/* Copyright (c) 2018 Big Ladder Software LLC. All rights reserved.
* See the LICENSE file for additional terms and conditions. */


// Standard
#include <iostream>

// vendor
#include "gtest/gtest.h"
#include "gmock/gmock.h"

// btwxt
#include <griddeddata.h>
#include <gridpoint.h>
#include "fixtures.hpp"

using namespace Btwxt;


TEST(GridPoint, constructor) {
    std::vector<double> target_vector{3.3, 4.4, 5.5};
    GridPoint test_target(target_vector);
    EXPECT_EQ(test_target.target, target_vector);
}

TEST_F(TwoDFixture, locator_basics) {
    GridPoint test_target(target);
    WhereInTheGridIsThisPoint test_locator(test_target, test_gridded_data);

    std::vector<std::size_t> point_floor = test_locator.get_floor();
    std::vector<std::size_t> expected_floor{1, 0};
    EXPECT_EQ(point_floor, expected_floor);

    std::vector<double> weights = test_locator.get_weights();
    std::vector<double> expected_weights{0.4, 0.5};
    EXPECT_EQ(weights, expected_weights);
}

TEST_F(TwoDFixture, locator_oobounds) {
    std::vector<double> oobounds_vector = {16, 3};
    GridPoint oobounds_target(oobounds_vector);
    WhereInTheGridIsThisPoint test_locator(oobounds_target, test_gridded_data);

    std::vector<std::size_t> point_floor = test_locator.get_floor();
    std::vector<std::size_t> expected_floor{1, 0};
    EXPECT_EQ(point_floor, expected_floor);

    std::vector<double> weights = test_locator.get_weights();
    std::vector<double> expected_weights{1.2, -0.5};
    EXPECT_EQ(weights, expected_weights);
};

TEST_F(TwoDFixture, locator_outlaw) {
    std::pair<double, double> extrap_limits{-2, 17};
    test_gridded_data.set_axis_extrap_limits(0, extrap_limits);
    std::vector<double> outlaw_vector{18, 3};
    GridPoint outlaw_target(outlaw_vector);
    std::string ExpectedOut = "  WARNING: The target is outside the extrapolation limits"
                              " in dimension 0. Will perform constant extrapolation.\n";
    EXPECT_STDOUT(WhereInTheGridIsThisPoint test_locator(outlaw_target, test_gridded_data);,
                  ExpectedOut);
};

TEST_F(TwoDFixture, locator_consolidate_methods) {
    GridPoint test_target(target);
    WhereInTheGridIsThisPoint test_locator(test_target, test_gridded_data);

    std::vector<Method> methods = test_locator.get_methods();
    std::vector<Method> expected_methods{Method::LINEAR, Method::LINEAR};
    EXPECT_EQ(methods, expected_methods);

    std::vector<double> oobounds_vector = {12,3};
    GridPoint oobounds_target(oobounds_vector);
    test_locator = WhereInTheGridIsThisPoint(oobounds_target, test_gridded_data);
    methods = test_locator.get_methods();
    expected_methods = {Method::LINEAR, Method::CONSTANT};
    EXPECT_EQ(methods, expected_methods);
};

TEST_F(TwoDFixture, locator_interp_coeffs) {
    GridPoint test_target(target);
    WhereInTheGridIsThisPoint test_locator(test_target, test_gridded_data);

    std::vector<std::vector<double> > interp_coeffs = test_locator.get_interp_coeffs();
    std::vector<std::vector<double> > cubic_slope_coeffs = test_locator.get_cubic_slope_coeffs();
    std::vector<double> mu = test_locator.get_weights();

    EXPECT_EQ(interp_coeffs[0][1], mu[0]);
    EXPECT_EQ(interp_coeffs[1][0], 1 - mu[1]);

    EXPECT_EQ(cubic_slope_coeffs[0][0], 0);
    EXPECT_EQ(cubic_slope_coeffs[1][1], 0);
};

TEST_F(CubicFixture, locator_interp_coeffs) {
    GridPoint current_grid_point(target);
    WhereInTheGridIsThisPoint test_locator(current_grid_point, test_gridded_data);

    std::vector<std::vector<double> > interp_coeffs = test_locator.get_interp_coeffs();
    std::vector<std::vector<double> > cubic_slope_coeffs = test_locator.get_cubic_slope_coeffs();
    double mu = test_locator.get_weights()[0];

    EXPECT_EQ(interp_coeffs[0][0], 2 * mu * mu * mu - 3 * mu * mu + 1);
    EXPECT_EQ(interp_coeffs[0][1], -2 * mu * mu * mu + 3 * mu * mu);

    EXPECT_EQ(cubic_slope_coeffs[0][0], mu * mu * mu - 2 * mu * mu + mu);
    EXPECT_EQ(cubic_slope_coeffs[0][1], mu * mu * mu - mu * mu);
};
