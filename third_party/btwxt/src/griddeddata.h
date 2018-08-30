/* Copyright (c) 2018 Big Ladder Software LLC. All rights reserved.
 * See the LICENSE file for additional terms and conditions. */

#ifndef GRIDDEDDATA_H_
#define GRIDDEDDATA_H_

#include <cfloat>
#include <vector>

namespace Btwxt {

enum class Method { CONSTANT, LINEAR, CUBIC, UNDEF };

class GridAxis {
  // A single input dimension of the performance space
public:
  GridAxis();

  explicit GridAxis(std::vector<double> grid_vector, Method extrapolation_method = Method::CONSTANT,
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

  double get_spacing_multiplier(const std::size_t &flavor, const std::size_t &index);

private:
  void calc_spacing_multipliers();
  void check_grid_sorted();
  void check_extrap_limits();
};

class GriddedData {
public:
  GriddedData();

  GriddedData(std::vector<std::vector<double>> grid, std::vector<std::vector<double>> values);

  GriddedData(std::vector<GridAxis> grid_axes, std::vector<std::vector<double>> values);

  explicit GriddedData(std::vector<GridAxis> grid_axes);

  std::size_t get_ndims();

  std::size_t get_num_tables();

  std::size_t add_value_table(std::vector<double> &value_vector);

  const std::vector<double> &get_grid_vector(const std::size_t &dim);

  std::pair<double, double> get_extrap_limits(const std::size_t &dim);

  std::size_t get_value_index(const std::vector<std::size_t> &coords);

  std::vector<double> get_values(const std::vector<std::size_t> &coords);

  std::vector<double> get_values_relative(const std::vector<std::size_t> &coords,
                                           const std::vector<short> &translation);

  double get_axis_spacing_mult(const std::size_t &dim, const std::size_t &flavor,
                               const std::size_t &index);

  std::vector<Method> get_interp_methods();

  std::vector<Method> get_extrap_methods();
  // double get_value(std::size_t table_index, std::vector<std::size_t> coords);

  void set_axis_extrap_method(const std::size_t &dim, Method);

  void set_axis_extrap_limits(const std::size_t &dim,
                              const std::pair<double, double> &extrap_limits);

  void set_axis_interp_method(const std::size_t &dim, Method);

  std::vector<std::vector<double>> value_tables;
  std::size_t num_values;
  std::vector<std::size_t> dimension_lengths;
  std::size_t num_tables;
  std::vector<GridAxis> grid_axes;
  std::size_t ndims;

private:
  void construct_axes(const std::vector<std::vector<double>> &grid);
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
