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

#define EXPECT_STDOUT(action, expected_stdout)                                                     \
    {                                                                                              \
        std::stringstream buffer;                                                                  \
        std::streambuf* sbuf = std::cout.rdbuf();                                                  \
        std::cout.rdbuf(buffer.rdbuf());                                                           \
        action std::string capture = buffer.str();                                                 \
        std::cout.rdbuf(sbuf);                                                                     \
        EXPECT_STREQ((expected_stdout).c_str(), buffer.str().c_str());                             \
    }

namespace Btwxt {

class GridFixture : public testing::Test {
  public:
    std::vector<std::vector<double>> grid;
    std::vector<std::vector<double>> data_sets;
    std::vector<double> target;
    RegularGridInterpolator interpolator;

    GridFixture() = default;

    virtual void setup() { interpolator = RegularGridInterpolator(grid, data_sets); }
};

class Grid2DFixture : public GridFixture {
  protected:
    Grid2DFixture()
    {
        grid = {{0, 10, 15}, {4, 6}};
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

class FunctionFixture : public GridFixture {
  public:
    std::vector<std::function<double(std::vector<double>)>> functions;

  protected:
    FunctionFixture() = default;

    void setup() override
    {
        data_sets.resize(functions.size());
        for (std::size_t i = 0u; i < functions.size(); i++) {
            for (auto& grid_point : cartesian_product(grid)) {
                data_sets[i].push_back(functions[i](grid_point));
            }
        }
        interpolator = RegularGridInterpolator(grid, data_sets);
    }
};

class Function2DFixture : public FunctionFixture {
  protected:
    Function2DFixture()
    {
        grid = {{2.0, 7.0}, {1.0, 2.0, 3.0}};
        functions = {[](std::vector<double> x) -> double { return x[0] * x[1]; }};
        setup();
    }
};

class Function4DFixture : public FunctionFixture {
  protected:
    Function4DFixture()
    {
        const std::size_t number_of_axes = 4;
        grid.resize(number_of_axes);

        std::size_t axis_len = 10; // could easily change to vector of lengths
        for (std::size_t i = 0; i < number_of_axes; i++) {
            grid[i] = linspace(0.0, 4.5, axis_len);
        }

        functions = {
            [](std::vector<double> x) -> double { return sin(x[0] + x[1]) + cos(x[2] + x[3]); },
            [](std::vector<double> x) -> double { return x[0] + x[1] + x[2] + x[3]; }};

        target = {2.2, 3.3, 1.4, 4.1};
        setup();
    }
};

} // namespace Btwxt