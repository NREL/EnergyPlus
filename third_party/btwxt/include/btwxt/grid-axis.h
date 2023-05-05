/* Copyright (c) 2018 Big Ladder Software LLC. All rights reserved.
 * See the LICENSE file for additional terms and conditions. */

//#pragma once
#ifndef GRID_AXIS_H_
#define GRID_AXIS_H_

// Standard
#include <algorithm>
#include <cfloat>
#include <memory>
#include <optional>
#include <string_view>
#include <vector>

// btwxt
#include "logging.h"

namespace Btwxt {

enum class Method { undefined, constant, linear, cubic };

class GridAxis {
    // A single input dimension of the grid
  public:
    // Constructors
    GridAxis() = default;

    explicit GridAxis(
        std::vector<double> values,
        const std::string& name = "",
        Method interpolation_method = Method::linear,
        Method extrapolation_method = Method::constant,
        std::pair<double, double> extrapolation_limits = {-DBL_MAX, DBL_MAX},
        const std::shared_ptr<Courierr::Courierr>& logger = std::make_shared<BtwxtLogger>());

    // Setters
    void set_interpolation_method(Method interpolation_method_in);
    void set_extrapolation_method(Method extrapolation_method_in);
    void set_extrapolation_limits(std::pair<double, double> limits)
    {
        extrapolation_limits = limits;
        check_extrapolation_limits();
    }

    void set_logger(std::shared_ptr<Courierr::Courierr> logger_in)
    {
        logger = std::move(logger_in);
    }
    std::shared_ptr<Courierr::Courierr> get_logger() { return logger; };

    // Getters
    [[nodiscard]] const std::vector<double>& get_values() const { return values; }
    [[nodiscard]] std::size_t get_length() const { return values.size(); }
    [[nodiscard]] Method get_extrapolation_method() const { return extrapolation_method; }
    [[nodiscard]] Method get_interpolation_method() const { return interpolation_method; }
    [[nodiscard]] std::pair<double, double> get_extrapolation_limits() const
    {
        return extrapolation_limits;
    }

    [[nodiscard]] const std::vector<double>&
    get_cubic_spacing_ratios(std::size_t floor_or_ceiling) const;
    std::string name;

  private:
    std::vector<double> values;
    Method extrapolation_method {Method::constant};
    Method interpolation_method {Method::linear};
    std::pair<double, double> extrapolation_limits {-DBL_MAX, DBL_MAX};
    std::vector<std::vector<double>>
        cubic_spacing_ratios; // Used for cubic interpolation. Outer vector is size 2: 0: spacing
                              // for the floor, 1: spacing for the ceiling. Inner vector is length
                              // of axis values, but the floor vector doesn't use the first entry
                              // and the ceiling doesn't use the last entry.
    std::shared_ptr<Courierr::Courierr> logger;
    void calculate_cubic_spacing_ratios();
    void check_grid_sorted();
    void check_extrapolation_limits();
};

// free functions
inline bool vector_is_sorted(const std::vector<double>& vector_in)
{
    return std::is_sorted(std::begin(vector_in),
                          std::end(vector_in),
                          [](const double a, const double b) { return a <= b; });
}

template <typename T>
std::vector<std::vector<T>> cartesian_product(const std::vector<std::vector<T>>& dimension_vectors)
{
    std::vector<std::vector<T>> combinations = {{}};
    for (const auto& list : dimension_vectors) {
        std::vector<std::vector<T>> r;
        for (const auto& x : combinations) {
            for (const auto item : list) {
                r.push_back(x);
                r.back().push_back(item);
            }
        }
        combinations = std::move(r);
    }
    return combinations;
}

std::vector<double> linspace(double start, double stop, std::size_t number_of_points);

} // namespace Btwxt

#endif // define GRID_AXIS_H_