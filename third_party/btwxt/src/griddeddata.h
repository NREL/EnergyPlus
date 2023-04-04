/* Copyright (c) 2018 Big Ladder Software LLC. All rights reserved.
 * See the LICENSE file for additional terms and conditions. */

#ifndef GRIDDEDDATA_H_
#define GRIDDEDDATA_H_

#include "error.h"
#include <cfloat>
#include <optional>
#include <vector>

namespace Btwxt {

enum class Method { CONSTANT, LINEAR, CUBIC, UNDEF };

class GridAxis {
  // A single input dimension of the performance space
public:
  GridAxis() = default;

  GridAxis(std::vector<double> grid_vector, const BtwxtLoggerFn *logger = nullptr,
           void *logger_context = nullptr, Method extrapolation_method = Method::CONSTANT,
           Method interpolation_method = Method::LINEAR,
           std::pair<double, double> extrapolation_limits = {-DBL_MAX, DBL_MAX});

  std::vector<double> grid;
  std::vector<std::vector<double>> spacing_multipliers;
  Method extrapolation_method;
  Method interpolation_method;
  std::pair<double, double> extrapolation_limits;

  // bool is_regular;  <-- to add later

  std::size_t get_length();

  void set_interp_method(Method interpolation_method);
  void set_extrap_method(Method extrapolation_method);
  void set_extrap_limits(std::pair<double, double> extrap_limits);

  double get_spacing_multiplier(const std::size_t &flavor, const std::size_t &index) const;

  void set_logging_callback(const BtwxtLoggerFn *callback_function, void *caller_info);

private:
  const BtwxtLoggerFn *callback_;
  void *callback_context_;

  void calc_spacing_multipliers();
  void check_grid_sorted();
  void check_extrap_limits();
};

class GriddedData {
public:
  GriddedData() = default;

  GriddedData(std::vector<std::vector<double>> grid, std::vector<std::vector<double>> values,
              const BtwxtLoggerFn *logger = nullptr, void *logger_context = nullptr);

  GriddedData(std::vector<std::vector<double>> grid, const BtwxtLoggerFn *logger = nullptr,
              void *logger_context = nullptr);

  GriddedData(std::vector<GridAxis> grid_axes, std::vector<std::vector<double>> values,
              const BtwxtLoggerFn *logger = nullptr, void *logger_context = nullptr);

  explicit GriddedData(std::vector<GridAxis> grid_axes, const BtwxtLoggerFn *logger = nullptr,
                       void *logger_context = nullptr);

  std::size_t get_ndims() const;

  std::size_t get_num_tables() const;

  std::size_t add_value_table(const std::vector<double> &value_vector);

  const std::vector<double> &get_grid_vector(const std::size_t &dim);

  std::pair<double, double> get_extrap_limits(const std::size_t &dim) const;

  std::size_t get_value_index(const std::vector<std::size_t> &coords) const;

  std::size_t get_value_index_relative(const std::vector<std::size_t> &coords,
                                       const std::vector<short> &translation);

  std::vector<double> get_values(const std::vector<std::size_t> &coords);

  std::vector<double> get_values_relative(const std::vector<std::size_t> &coords,
                                          const std::vector<short> &translation);

  std::vector<double> get_values(const std::size_t index);

  double get_axis_spacing_mult(const std::size_t &dim, const std::size_t &flavor,
                               const std::size_t &index) const;

  std::vector<Method> get_interp_methods() const;

  std::vector<Method> get_extrap_methods() const;

  void normalize_value_table(std::size_t table_num, double scalar = 1.0);

  void set_axis_extrap_method(const std::size_t &dim, Method);

  void set_axis_extrap_limits(const std::size_t &dim,
                              const std::pair<double, double> &extrap_limits);

  void set_axis_interp_method(const std::size_t &dim, Method);

  std::string write_data();

  void set_logging_callback(const BtwxtLoggerFn *callback_function, void *caller_info);

  std::vector<std::vector<double>> value_tables;
  std::size_t num_values;
  std::size_t num_tables;
  std::vector<GridAxis> grid_axes;
  std::size_t ndims;
  std::vector<std::size_t> dimension_lengths;

private:
  void construct_axes(const std::vector<std::vector<double>> &grid);
  void set_dimension_sizes();
  std::vector<std::size_t> dimension_step_size;
  std::vector<std::size_t> temp_coords;
  std::vector<double> results;
};

// free functions
bool free_check_sorted(std::vector<double>);

template <typename T>
std::vector<std::vector<T>> cart_product(const std::vector<std::vector<T>> &v) {
  std::vector<std::vector<T>> combinations = {{}};
  for (const auto &list : v) {
    std::vector<std::vector<T>> r;
    for (const auto &x : combinations) {
      for (const auto item : list) {
        r.push_back(x);
        r.back().push_back(item);
      }
    }
    combinations = std::move(r);
  }
  return combinations;
}

} // namespace Btwxt
#endif // GRIDDEDDATA_H_
