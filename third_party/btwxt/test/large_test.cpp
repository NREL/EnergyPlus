/* Copyright (c) 2018 Big Ladder Software LLC. All rights reserved.
 * See the LICENSE file for additional terms and conditions. */

// Standard
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include <chrono>
#include <iostream>

// btwxt
#include <btwxt.h>
#include <error.h>
#include <griddeddata.h>

using namespace Btwxt;

// return an evenly spaced 1-d grid of doubles.
std::vector<double> linspace(double first, double last, std::size_t len) {
  std::vector<double> result(len);
  double step = (last - first) / (len - 1);
  double val = first;
  for (std::size_t i = 0; i < len; i++, val += step) {
    result[i] = val;
  }
  return result;
}

// the functions to interpolate.
double fn0(double x0, double x1, double x2, double x3) { return sin(x0 + x1) + cos(x2 + x3); }

double fn1(double x0, double x1, double x2, double x3) { return (x0 + x1 + x2 + x3); }

class LargeFixture : public testing::Test {
protected:
  RegularGridInterpolator test_rgi;
  std::vector<double> target;

  LargeFixture() {
    const std::size_t ndims = 4;
    std::vector<std::vector<double>> grid(ndims);

    std::size_t axis_len = 10; // could easily change to vector of lengths
    std::size_t num_values = 1;
    std::vector<std::size_t> dim_lengths;
    for (std::size_t i = 0; i < ndims; i++) {
      grid[i] = linspace(0.0, 4.5, axis_len);
      num_values *= grid[i].size();
      dim_lengths.push_back(grid[i].size());
    }

    std::vector<double> values0(num_values);
    std::vector<double> values1(num_values);
    std::size_t value_loc = 0;
    for (std::size_t i0 = 0; i0 < dim_lengths[0]; i0++) {
      for (std::size_t i1 = 0; i1 < dim_lengths[1]; i1++) {
        for (std::size_t i2 = 0; i2 < dim_lengths[2]; i2++) {
          for (std::size_t i3 = 0; i3 < dim_lengths[3]; i3++) {
            values0[value_loc] = fn0(grid[0][i0], grid[1][i1], grid[2][i2], grid[3][i3]);
            values1[value_loc] = fn1(grid[0][i0], grid[1][i1], grid[2][i2], grid[3][i3]);
            value_loc++;
          }
        }
      }
    }
    std::vector<std::vector<double>> values = {values0, values1};

    // target = {2.5, 3.5, 1.4, 4.0};
    target = {2.2, 3.3, 1.4, 4.1};
    // target = {0.0, 0.0, 0.0, 0.0};
    ////test_gridded_data = GriddedData(grid, values);
    //        test_gridded_data.set_axis_interp_method(0, Method::CUBIC);
    //        test_gridded_data.set_axis_interp_method(1, Method::CUBIC);
    //        test_gridded_data.set_axis_interp_method(2, Method::CUBIC);
    //        test_gridded_data.set_axis_interp_method(3, Method::CUBIC);
    test_rgi = RegularGridInterpolator(grid, values);
  }
};

TEST_F(LargeFixture, construct) {
  test_rgi.set_new_target(target);

  std::vector<double> returned_target = test_rgi.get_current_target();
  EXPECT_THAT(returned_target, testing::ElementsAre(2.2, 3.3, 1.4, 4.1));
}

TEST_F(LargeFixture, calculate) {
  test_rgi.set_new_target(target);

  std::vector<double> result = test_rgi.get_values_at_target();
  EXPECT_NEAR(result[0], fn0(target[0], target[1], target[2], target[3]), 0.02);
  EXPECT_DOUBLE_EQ(result[1], fn1(target[0], target[1], target[2], target[3]));
}

TEST_F(LargeFixture, verify_linear) {
  // no matter what we do, result[1] should always be 11!
  std::vector<double> result;

  test_rgi.set_new_target(target);
  result = test_rgi.get_values_at_target();
  EXPECT_DOUBLE_EQ(result[1], 11);

  test_rgi.set_axis_interp_method(0, Method::CUBIC);
  test_rgi.set_new_target(target);
  result = test_rgi.get_values_at_target();
  EXPECT_DOUBLE_EQ(result[1], 11);

  test_rgi.set_axis_interp_method(3, Method::CUBIC);
  test_rgi.set_new_target(target);
  result = test_rgi.get_values_at_target();
  EXPECT_DOUBLE_EQ(result[1], 11);

  test_rgi.set_axis_interp_method(0, Method::LINEAR);
  test_rgi.set_new_target(target);
  result = test_rgi.get_values_at_target();
  EXPECT_DOUBLE_EQ(result[1], 11);

  test_rgi.set_axis_interp_method(2, Method::CUBIC);
  test_rgi.set_new_target(target);
  result = test_rgi.get_values_at_target();
  EXPECT_DOUBLE_EQ(result[1], 11);

  test_rgi.set_axis_interp_method(0, Method::CUBIC);
  test_rgi.set_new_target(target);
  result = test_rgi.get_values_at_target();
  EXPECT_DOUBLE_EQ(result[1], 11);

  test_rgi.set_axis_interp_method(1, Method::CUBIC);
  test_rgi.set_new_target(target);
  result = test_rgi.get_values_at_target();
  EXPECT_DOUBLE_EQ(result[1], 11);
}

TEST_F(LargeFixture, timer) {
  test_rgi.set_new_target(target);

  // Get starting timepoint
  auto start = std::chrono::high_resolution_clock::now();
  std::vector<double> result = test_rgi.get_values_at_target();
  // Get ending timepoint
  auto stop = std::chrono::high_resolution_clock::now();
  auto duration = std::chrono::duration_cast<std::chrono::microseconds>(stop - start);
  showMessage(MsgLevel::MSG_INFO,
              stringify("Time taken by interpolation: ", duration.count(), " microseconds"));

  // time running the functions straight
  start = std::chrono::high_resolution_clock::now();
  // double r0 = fn0(target[0], target[1], target[2], target[3]);
  // double r1 = fn1(target[0], target[1], target[2], target[3]);
  // Get ending timepoint
  stop = std::chrono::high_resolution_clock::now();
  auto nano_duration = std::chrono::duration_cast<std::chrono::nanoseconds>(stop - start);
  showMessage(MsgLevel::MSG_INFO,
              stringify("Time taken by direct functions: ", nano_duration.count(), " nanoseconds"));
}

TEST_F(LargeFixture, multi_timer) {
  std::vector<std::vector<double>> set_of_targets = {
      {0.1, 0.1, 0.1, 0.1}, {3.3, 2.2, 4.1, 1.4}, {2.1, 1.6, 1.6, 2.1}, {3.7, 4.3, 0.8, 2.1},
      {1.9, 3.4, 1.2, 1.1}, {3.3, 3.8, 1.6, 3.0}, {0.3, 1.0, 2.4, 1.1}, {3.1, 1.9, 2.9, 3.3},
      {4.2, 2.7, 1.3, 4.4}, {2.1, 2.9, 1.8, 1.9}};

  for (std::size_t count = 0; count < 10; count++) {
    // Get starting timepoint
    auto start = std::chrono::high_resolution_clock::now();
    for (auto target : set_of_targets) {
      std::vector<double> result = test_rgi(target);
    }
    // Get ending timepoint
    auto stop = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::microseconds>(stop - start);
    showMessage(MsgLevel::MSG_INFO,
                stringify("Time taken by ten interpolations: ", duration.count(), " microseconds"));
  }
}
