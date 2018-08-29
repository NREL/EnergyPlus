/* Copyright (c) 2018 Big Ladder Software LLC. All rights reserved.
 * See the LICENSE file for additional terms and conditions. */

// Standard
#include <iostream>

// vendor
#include "gmock/gmock.h"
#include "gtest/gtest.h"

// btwxt
#include "fixtures.hpp"
#include <btwxt.h>
#include <error.h>
#include <griddeddata.h>

using namespace Btwxt;

TEST_F(ThreeDFixture, hypercube) {
  auto hypercube = test_rgi.get_hypercube();
  EXPECT_EQ(hypercube.size(), 16u); }

TEST_F(ThreeDFixture, test_hypercube) {
  auto hypercube = test_rgi.get_hypercube();
  EXPECT_EQ(hypercube.size(), 2u * 4u * 2u);
  EXPECT_THAT(hypercube[0], testing::ElementsAre(0, -1, 0));
  EXPECT_THAT(hypercube[2], testing::ElementsAre(0, 0, 0));
  EXPECT_THAT(hypercube[12], testing::ElementsAre(1, 1, 0));
  EXPECT_THAT(hypercube[15], testing::ElementsAre(1, 2, 1));
}

TEST_F(ThreeDFixture, make_linear_hypercube) {
  test_rgi.set_axis_interp_method(1, Method::LINEAR);
  auto hypercube = test_rgi.get_hypercube();
  EXPECT_EQ(hypercube.size(), 8u);
  EXPECT_THAT(hypercube[0], testing::ElementsAre(0, 0, 0));
  EXPECT_THAT(hypercube[2], testing::ElementsAre(0, 1, 0));
  EXPECT_THAT(hypercube[5], testing::ElementsAre(1, 0, 1));
}

TEST(Hypercube, cart_product) {
  std::vector<std::vector<short>> v = {{1, 2, 3}, {4, 5}, {6, 7, 8, 9}};
  std::vector<std::vector<short>> result = cart_product(v);
  EXPECT_EQ(result.size(), 3u * 2u * 4u);
  EXPECT_THAT(result[0], testing::ElementsAre(1, 4, 6));
  EXPECT_THAT(result[1], testing::ElementsAre(1, 4, 7));
  EXPECT_THAT(result[10], testing::ElementsAre(2, 4, 8));
  EXPECT_THAT(result[3 * 2 * 4 - 1], testing::ElementsAre(3, 5, 9));
}

TEST_F(CubicFixture, hypercube_weigh_one_vertex) {
  test_gridded_data.set_axis_interp_method(1, Method::CUBIC);
  GridPoint grid_point(test_gridded_data, target);
  std::vector<Method> methods = grid_point.get_methods();
  std::vector<std::vector<double>> spacing_mults = grid_point.get_spacing_mults(test_gridded_data);

  std::vector<double> mus = grid_point.get_weights();
  double mx = mus[0];
  double my = mus[1];
  double c0x = 2 * mx * mx * mx - 3 * mx * mx + 1;
  double c0y = 2 * my * my * my - 3 * my * my + 1;
  // double c1x = -2*mx*mx*mx + 3*mx*mx;
  double c1y = -2 * my * my * my + 3 * my * my;
  double d0x = mx * mx * mx - 2 * mx * mx + mx;
  double d0y = my * my * my - 2 * my * my + my;
  double d1x = mx * mx * mx - mx * mx;
  double d1y = my * my * my - my * my;
  double s1x = 5.0 / 10;
  double s1y = 2.0 / 4;
  double s0x = 5.0 / 9;
  double s0y = 2.0 / 4;

  std::vector<short> this_vertex = {0, 0};
  double weight = grid_point.get_vertex_weight(this_vertex, spacing_mults);
  double expected_result = c0x * c0y;
  expected_result += -1 * c0x * d1y * s1y;
  expected_result += -1 * d1x * s1x * c0y;
  expected_result += d1x * s1x * d1y * s1y;
  EXPECT_DOUBLE_EQ(weight, expected_result);

  this_vertex = {-1, 1};
  weight = grid_point.get_vertex_weight(this_vertex, spacing_mults);
  expected_result = -1 * d0x * s0x * c1y;
  expected_result += -1 * d0x * s0x * d0y * s0y;
  EXPECT_DOUBLE_EQ(weight, expected_result);

  this_vertex = {2, 0};
  weight = grid_point.get_vertex_weight(this_vertex, spacing_mults);
  expected_result = d1x * s1x * c0y;
  expected_result += -1 * d1x * s1x * d1y * s1y;
  EXPECT_DOUBLE_EQ(weight, expected_result);

  this_vertex = {2, 2};
  weight = grid_point.get_vertex_weight(this_vertex, spacing_mults);
  expected_result = d1x * s1x * d1y * s1y;
  EXPECT_DOUBLE_EQ(weight, expected_result);
}

TEST_F(CubicFixture, hypercube_calculations) {
  test_gridded_data.set_axis_interp_method(1, Method::CUBIC);
  GridPoint grid_point(test_gridded_data, target);

  std::vector<double> result = grid_point.get_results();
  EXPECT_NEAR(result[0], 4.1953, 0.0001);
  EXPECT_NEAR(result[1], 11.9271, 0.0001);
}

TEST_F(OneDFixture, hypercube_calculations) {
  test_gridded_data.set_axis_interp_method(0, Method::CUBIC);
  GridPoint grid_point(test_gridded_data, target);
  std::vector<Method> methods = grid_point.get_methods();

  std::vector<double> result = grid_point.get_results();
  EXPECT_NEAR(result[0], 4.804398, 0.00001);
}
