/* Copyright (c) 2018 Big Ladder Software LLC. All rights reserved.
 * See the LICENSE file for additional terms and conditions. */

// Standard
#include <iostream>
#include <memory>

// Vendor
#include <gmock/gmock.h>
#include <gtest/gtest.h>

// btwxt
#include <btwxt/btwxt.h>

// testing
#include "fixtures/public-fixtures.h" // EXPECT_STDOUT

namespace Btwxt {

TEST(GridAxis, vector_is_sorted)
{
    std::vector<std::pair<std::vector<double>, bool>> axes = {{{1, 3, 5, 7, 9}, true},
                                                              {{1, 3, 5, 17, 9}, false},
                                                              {{9, 7, 5, 3, 1}, false},
                                                              {{1, 3, 3, 7, 9}, false},
                                                              {{9}, true}};
    bool is_sorted;
    for (const auto& pair : axes) {
        is_sorted = Btwxt::vector_is_sorted(pair.first);
        EXPECT_EQ(is_sorted, pair.second);
    }
}

TEST(GridAxis, sorting)
{
    std::vector<double> grid_vector = {0, 5, 7, 17, 15};
    EXPECT_THROW(GridAxis my_grid_axis = GridAxis(grid_vector), BtwxtException);
    grid_vector = {0, 5, 7, 10, 15};
    EXPECT_NO_THROW(GridAxis my_grid_axis = GridAxis(grid_vector););
}

TEST(GridAxis, calculate_cubic_spacing_ratios)
{
    static constexpr std::size_t floor = 0;
    static constexpr std::size_t ceiling = 1;

    GridAxis grid_axis({6., 10., 15., 20., 22.},
                       "",
                       Method::cubic,
                       Method::constant,
                       {-DBL_MAX, DBL_MAX},
                       std::make_shared<BtwxtLogger>());
    EXPECT_THAT(grid_axis.get_cubic_spacing_ratios(floor),
                testing::ElementsAre(1, 5.0 / 9, 0.5, 2.0 / 7));
    EXPECT_THAT(grid_axis.get_cubic_spacing_ratios(ceiling),
                testing::ElementsAre(4.0 / 9, 0.5, 5.0 / 7, 1));
}

TEST(GridAxis, bad_limits)
{
    GridAxis my_grid_axis({0, 5, 7, 11, 12, 15});
    my_grid_axis.name = "my grid axis";
    std::pair<double, double> extrapolation_limits {4, 17};
    std::string expected_out =
        "  [NOTE] Grid axis (name=\"my grid axis\") lower extrapolation limit (4) is within the "
        "set of grid axis values. Setting to smallest axis value (0).\n";
    EXPECT_STDOUT(my_grid_axis.set_extrapolation_limits(extrapolation_limits);, expected_out)
    EXPECT_EQ(my_grid_axis.get_extrapolation_limits().first, 0);

    extrapolation_limits = {-2, 12};
    expected_out =
        "  [NOTE] Grid axis (name=\"my grid axis\") upper extrapolation limit (12) is within the "
        "set of grid axis values. Setting to largest axis value (15).\n";
    EXPECT_STDOUT(my_grid_axis.set_extrapolation_limits(extrapolation_limits);, expected_out)
    EXPECT_EQ(my_grid_axis.get_extrapolation_limits().second, 15);
}
} // namespace Btwxt