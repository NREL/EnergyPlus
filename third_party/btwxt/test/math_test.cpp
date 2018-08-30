/* Copyright (c) 2018 Big Ladder Software LLC. All rights reserved.
 * See the LICENSE file for additional terms and conditions. */

// Standard
#include <iostream>

// vendor
#include "gmock/gmock.h"
#include "gtest/gtest.h"

// btwxt
#include <btwxt.h>
#include <error.h>
#include <griddeddata.h>

using namespace Btwxt;

// Unit tests on free functions, generally mathematically oriented.

TEST(GriddedData, free_check_sorted) {
  Btwxt::LOG_LEVEL = 0;
  std::vector<std::pair<std::vector<double>, bool>> my_vecs = {{{1, 3, 5, 7, 9}, true},
                                                               {{1, 3, 5, 17, 9}, false},
                                                               {{9, 7, 5, 3, 1}, false},
                                                               {{1, 3, 3, 7, 9}, false},
                                                               {{9}, true}};
  bool is_sorted;
  for (auto pair : my_vecs) {
    is_sorted = Btwxt::free_check_sorted(pair.first);
    showMessage(MsgLevel::MSG_DEBUG, (is_sorted ? "true" : "false"));
    EXPECT_EQ(is_sorted, pair.second);
  }
  Btwxt::LOG_LEVEL = 1;
}

TEST(GriddedData, locate_coords) {
  std::vector<std::vector<double>> grid = {{1, 2, 3, 4, 5}, {1, 2, 3, 4, 5, 6, 7}};
  std::vector<double> value1(5 * 7, 0.0);
  std::vector<std::vector<double>> values = {value1};
  GriddedData test_gridded_data1(grid, values);

  std::vector<std::size_t> coords = {2, 3};
  std::vector<std::size_t> dimension_lengths = {5, 7};
  std::size_t index = test_gridded_data1.get_value_index(coords);
  EXPECT_EQ(index, 17u);

  coords = {2, 3, 2};
  grid = {{1, 2, 3, 4, 5}, {1, 2, 3, 4, 5, 6, 7}, {1, 2, 3}};

  std::vector<double> value2(5 * 7 * 3, 0.0);
  values = {value2};
  GriddedData test_gridded_data2(grid, values);
  index = test_gridded_data2.get_value_index(coords);
  EXPECT_EQ(index, 53u);
}

TEST(GridPoint, set_dim_floor) {

  GridAxis axis({1, 3, 5, 7, 9});

  axis.set_extrap_limits({0, 11});

  std::vector<double> target = {5.3};

  GriddedData test_gridded_data = GriddedData({axis});

  GridPoint grid_point(test_gridded_data, target);

  grid_point.set_floor();

  EXPECT_EQ(grid_point.get_is_inbounds()[0], Bounds::INBOUNDS);
  EXPECT_EQ(grid_point.get_floor()[0], 2u);
  double expected_weight = 0.15;
  double edge[2] = {axis.grid[grid_point.get_floor()[0]], axis.grid[grid_point.get_floor()[0] + 1]};
  double returned_weight = compute_fraction(target[0], edge);
  EXPECT_DOUBLE_EQ(returned_weight, expected_weight);

  grid_point.set_target({0.3});
  grid_point.set_floor();
  EXPECT_EQ(grid_point.get_is_inbounds()[0], Bounds::OUTBOUNDS);
  EXPECT_EQ(grid_point.get_floor()[0], 0u);

  grid_point.set_target({10.3});
  grid_point.set_floor();
  EXPECT_EQ(grid_point.get_is_inbounds()[0], Bounds::OUTBOUNDS);
  EXPECT_EQ(grid_point.get_floor()[0], 3u);

  grid_point.set_target({-0.3});
  grid_point.set_floor();
  EXPECT_EQ(grid_point.get_is_inbounds()[0], Bounds::OUTLAW);
  EXPECT_EQ(grid_point.get_floor()[0], 0u);

  grid_point.set_target({11.3});
  grid_point.set_floor();
  EXPECT_EQ(grid_point.get_is_inbounds()[0], Bounds::OUTLAW);
  EXPECT_EQ(grid_point.get_floor()[0], 3u);

  test_gridded_data.grid_axes[0].set_extrap_limits({-DBL_MAX, DBL_MAX});
  grid_point.set_target({-0.3});
  grid_point.set_floor();
  EXPECT_EQ(grid_point.get_is_inbounds()[0], Bounds::OUTBOUNDS);
  EXPECT_EQ(grid_point.get_floor()[0], 0u);

  grid_point.set_target({11.3});
  grid_point.set_floor();
  EXPECT_EQ(grid_point.get_is_inbounds()[0], Bounds::OUTBOUNDS);
  EXPECT_EQ(grid_point.get_floor()[0], 3u);
}

TEST(GridPoint, compute_fraction) {
  double x = 4.3;
  double edge[2] = {4, 6};
  double weight = compute_fraction(x, edge);
  EXPECT_DOUBLE_EQ(weight, 0.15);
}
