/* Copyright (c) 2018 Big Ladder Software LLC. All rights reserved.
 * See the LICENSE file for additional terms and conditions. */

#pragma once

// Standard
#include <memory>

// Vendor
#include <gtest/gtest.h>
#include <fmt/format.h>

// btwxt
#include "regular-grid-interpolator-implementation.h"
#include <btwxt/btwxt.h>

namespace Btwxt {

class GridImplementationFixture : public testing::Test {
  public:
    std::vector<std::vector<double>> grid;
    std::vector<std::vector<double>> data_sets;
    std::vector<double> target;
    RegularGridInterpolatorImplementation interpolator;

    GridImplementationFixture() = default;

    void setup()
    {
        auto logger = std::make_shared<BtwxtLogger>();
        interpolator = RegularGridInterpolatorImplementation(
            construct_grid_axes(grid, logger), construct_grid_point_data_sets(data_sets), logger);
    }
};

class Grid2DImplementationFixture : public GridImplementationFixture {
  protected:
    Grid2DImplementationFixture()
    {
        grid = grid = {{0, 10, 15}, {4, 6}};
        //         4  6
        data_sets = {{6,
                      3, // 0
                      2,
                      8, // 10
                      4,
                      2}, // 15
                     {12,
                      6, // 0
                      4,
                      16, // 10
                      8,
                      4}}; // 15
        target = {12, 5};
        setup();
        interpolator.set_axis_extrapolation_method(0, Method::linear);
    }
};

class CubicImplementationFixture : public GridImplementationFixture {
  protected:
    CubicImplementationFixture()
    {
        grid = {{6, 10, 15, 20}, {2, 4, 6, 8}};
        data_sets =
            //  2   4   6    8
            {{4,
              3,
              1.5,
              1, // 6
              5,
              4,
              2,
              1, // 10
              8,
              6,
              3,
              2, // 15
              10,
              8,
              4,
              2}, // 20

             {12,
              10,
              4,
              4, // 6
              16,
              12,
              6,
              4, // 10
              20,
              16,
              8,
              4, // 15
              25,
              20,
              10,
              5}}; // 20

        target = {12, 4.5};
        setup();
        interpolator.set_axis_interpolation_method(0, Method::cubic);
    }
};

class EmptyGridImplementationFixture : public testing::Test {
  protected:
  public:
    std::vector<std::vector<double>> grid;
    RegularGridInterpolatorImplementation interpolator;

    EmptyGridImplementationFixture() = default;

    void setup()
    {
        auto logger = std::make_shared<BtwxtLogger>();
        interpolator =
            RegularGridInterpolatorImplementation(construct_grid_axes(grid, logger), logger);
    }
};

class Grid3DImplementationFixture : public GridImplementationFixture {
  protected:
    Grid3DImplementationFixture()
    {
        grid = {{-15, 0.2, 105}, {0, 10, 15}, {4, 6}};
        data_sets = {{6, 3, 2, 8, 4, 2, 3, 6, 13, 2, 0, 15, 3, 6, 13, 2, 0, 15}};
        target = {26.9, 12, 5};
        setup();
        interpolator.set_axis_interpolation_method(0, Method::linear);
        interpolator.set_axis_interpolation_method(1, Method::cubic);
        interpolator.set_axis_interpolation_method(2, Method::linear);
    }
};

} // namespace Btwxt