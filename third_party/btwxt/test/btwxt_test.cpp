/* Copyright (c) 2018 Big Ladder Software LLC. All rights reserved.
 * See the LICENSE file for additional terms and conditions. */

// Standard
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include <chrono>
#include <iostream>

// btwxt
#include "fixtures.hpp"
#include <btwxt.h>
#include <error.h>
#include <griddeddata.h>

using namespace Btwxt;

TEST_F(TwoDFixture, construct_from_gridded_data) {
  Btwxt::LOG_LEVEL = 0;
  RegularGridInterpolator rgi_from_grid(test_gridded_data);
  std::size_t ndims = rgi_from_grid.get_ndims();
  EXPECT_EQ(ndims, 2u);
  Btwxt::LOG_LEVEL = 1;
};

TEST_F(TwoDFixture, target_undefined) {
  std::vector<double> returned_target;
  std::vector<std::size_t> bad_floor;
  double bad_result;
  std::string TargetExpectedOut =
      "  WARNING: The current target was requested, but no target has been set.\n";
  std::string ResultsExpectedOut =
      "  WARNING: Results were requested, but no target has been set.\n";

  // The test fixture does not instantiate a GridPoint.
  EXPECT_STDOUT(returned_target = test_rgi.get_current_target();, TargetExpectedOut);
  std::vector<double> expected_result = {0, 0};
  EXPECT_EQ(returned_target, expected_result);

  EXPECT_STDOUT(bad_result = test_rgi.get_value_at_target(0);, ResultsExpectedOut);
  EXPECT_EQ(bad_result, 0);

  // Define the target; make sure it works now.
  test_rgi.set_new_target(target);
  std::string EmptyOut = "";
  EXPECT_STDOUT(returned_target = test_rgi.get_current_target();, EmptyOut);
  expected_result = {12, 5};
  EXPECT_EQ(returned_target, expected_result);

  // Clear the target; see that it reverts to warnings.
  test_rgi.clear_current_target();
  EXPECT_STDOUT(returned_target = test_rgi.get_current_target();, TargetExpectedOut);
  expected_result = {0, 0};
  EXPECT_EQ(returned_target, expected_result);

  EXPECT_STDOUT(bad_result = test_rgi.get_value_at_target(0);, ResultsExpectedOut);
  EXPECT_EQ(bad_result, 0);
}

TEST_F(CubicFixture, spacing_multiplier) {
  double result;
  result = test_gridded_data.get_axis_spacing_mult(0, 0, 0);
  EXPECT_DOUBLE_EQ(result, 1.0);

  result = test_gridded_data.get_axis_spacing_mult(0, 1, 0);
  EXPECT_DOUBLE_EQ(result, (10 - 6) / (15.0 - 6.0));

  result = test_gridded_data.get_axis_spacing_mult(0, 0, 1);
  EXPECT_DOUBLE_EQ(result, (15 - 10) / (15.0 - 6.0));

  result = test_gridded_data.get_axis_spacing_mult(0, 1, 2);
  EXPECT_DOUBLE_EQ(result, 1.0);

  result = test_gridded_data.get_axis_spacing_mult(1, 0, 0);
  EXPECT_DOUBLE_EQ(result, 0.0);
}

TEST_F(CubicFixture, interpolate) {
  test_rgi.set_new_target(target);

  Btwxt::LOG_LEVEL = 0;
  auto start = std::chrono::high_resolution_clock::now();
  std::vector<double> result = test_rgi.get_values_at_target();
  auto stop = std::chrono::high_resolution_clock::now();
  auto duration = std::chrono::duration_cast<std::chrono::microseconds>(stop - start);
  showMessage(MsgLevel::MSG_INFO,
              stringify("Time to do cubic interpolation: ", duration.count(), " microseconds"));
  Btwxt::LOG_LEVEL = 1;
  EXPECT_THAT(result, testing::ElementsAre(testing::DoubleEq(4.158), testing::DoubleEq(11.836)));
}

TEST_F(TwoDFixture, interpolate) {
  Btwxt::LOG_LEVEL = 0;
  test_rgi.set_new_target(target);

  // All values, current target
  std::vector<double> result = test_rgi.get_values_at_target();
  EXPECT_THAT(result, testing::ElementsAre(testing::DoubleEq(4.2), testing::DoubleEq(8.4)));
  Btwxt::LOG_LEVEL = 1;
  // Single value, current target
  double d_result = test_rgi.get_value_at_target(0);
  EXPECT_DOUBLE_EQ(d_result, 4.2);

  std::vector<double> another_target = {8.1, 4.2};
  // All values, fresh target
  result = test_rgi.get_values_at_target(another_target);
  EXPECT_THAT(result, testing::ElementsAre(testing::DoubleEq(3.189), testing::DoubleEq(6.378)));
  // Single value, fresh target
  d_result = test_rgi.get_value_at_target(another_target, 1);
  EXPECT_DOUBLE_EQ(d_result, 6.378);
};

TEST_F(TwoDFixture, extrapolate) {
  // axis1 is designated constant extrapolation
  std::vector<double> const_extr_target = {10, 3};
  Btwxt::LOG_LEVEL = 0;
  std::vector<double> result = test_rgi(const_extr_target);
  EXPECT_THAT(result, testing::ElementsAre(testing::DoubleEq(2), testing::DoubleEq(4)));
  Btwxt::LOG_LEVEL = 1;

  // axis0 is designated linear extrapolation
  std::vector<double> lin_extr_target = {18, 5};
  Btwxt::LOG_LEVEL = 0;
  result = test_rgi(lin_extr_target);
  EXPECT_THAT(result, testing::ElementsAre(testing::DoubleEq(1.8), testing::DoubleEq(3.6)));
  Btwxt::LOG_LEVEL = 1;
};

TEST_F(TwoDFixture, invalid_inputs) {
  // we expect two errors that the value table inputs do not match the grid
  // we expect an error that the target dimensions do not match the grid
  std::vector<double> short_values = {6, 3, 2, 8, 4};
  EXPECT_THROW(test_gridded_data.add_value_table(short_values);, std::invalid_argument);
  std::vector<double> long_values = {1, 1, 1, 1, 1, 1, 1};
  EXPECT_THROW(test_gridded_data.add_value_table(long_values);, std::invalid_argument);

  std::vector<double> short_target = {1};
  EXPECT_THROW(test_rgi.set_new_target(short_target);, std::invalid_argument);
  std::vector<double> long_target = {1, 2, 3};
  EXPECT_THROW(test_rgi.set_new_target(long_target);, std::invalid_argument);
};

TEST_F(OneDFixture, cubic_interpolate) {
  Btwxt::LOG_LEVEL = 0;
  test_gridded_data.set_axis_interp_method(0, Method::CUBIC);
  test_rgi = RegularGridInterpolator(test_gridded_data);
  double result = test_rgi.get_values_at_target(target)[0];
  Btwxt::LOG_LEVEL = 1;
  EXPECT_NEAR(result, 4.804398, 0.0001);
}

TEST_F(TwoDFixture, cubic_interpolate) {
  Btwxt::LOG_LEVEL = 0;
  test_gridded_data.set_axis_interp_method(0, Method::CUBIC);
  test_gridded_data.set_axis_interp_method(1, Method::CUBIC);
  test_rgi = RegularGridInterpolator(test_gridded_data);
  test_rgi.set_new_target(target);

  // All values, current target
  std::vector<double> result = test_rgi.get_values_at_target();
  EXPECT_THAT(result, testing::ElementsAre(testing::DoubleEq(4.416), testing::DoubleEq(8.832)));
  Btwxt::LOG_LEVEL = 1;
}
