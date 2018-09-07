/* Copyright (c) 2018 Big Ladder Software LLC. All rights reserved.
 * See the LICENSE file for additional terms and conditions. */

// Standard
#include <iostream>

// btwxt
#include "error.h"
#include "griddeddata.h"

namespace Btwxt {

GridAxis::GridAxis() = default;

GridAxis::GridAxis(std::vector<double> grid_vector, Method extrapolation_method,
                   Method interpolation_method, std::pair<double, double> extrapolation_limits)
    : grid(std::move(grid_vector)),
      spacing_multipliers(2, std::vector<double>(grid.size() - 1, 1.0)),
      extrapolation_method(extrapolation_method),
      interpolation_method(interpolation_method),
      extrapolation_limits(std::move(extrapolation_limits)) {
  check_grid_sorted();
  check_extrap_limits();
  if (interpolation_method == Method::CUBIC) {
    calc_spacing_multipliers();
  }
}

std::size_t GridAxis::get_length() { return grid.size(); }

void GridAxis::set_interp_method(const Method im) {
  interpolation_method = im;
  if (im == Method::CUBIC) {
    calc_spacing_multipliers();
  }
}

void GridAxis::set_extrap_method(const Method em) { extrapolation_method = em; }

void GridAxis::set_extrap_limits(const std::pair<double, double> extrap_limits) {
  extrapolation_limits = extrap_limits;
  check_extrap_limits();
}

double GridAxis::get_spacing_multiplier(const std::size_t &flavor, const std::size_t &index) {
  return spacing_multipliers[flavor][index];
}

void GridAxis::calc_spacing_multipliers() {
  // "0" and "1" are the "flavors" of the calc_spacing_multipliers.
  // If you are sitting at the "0" along an edge of the hypercube, you want the "0" flavof
  double center_spacing;
  for (std::size_t i = 0; i < grid.size() - 1; i++) {
    center_spacing = grid[i + 1] - grid[i];
    if (i != 0) {
      spacing_multipliers[0][i] = center_spacing / (grid[i + 1] - grid[i - 1]);
    }
    if (i + 2 != grid.size()) {
      spacing_multipliers[1][i] = center_spacing / (grid[i + 2] - grid[i]);
    }
  }
}

void GridAxis::check_grid_sorted() {
  bool grid_is_sorted = Btwxt::free_check_sorted(grid);
  if (!grid_is_sorted) {
    showMessage(MsgLevel::MSG_ERR, "axis is not sorted.");
  }
}

void GridAxis::check_extrap_limits() {
  if (extrapolation_limits.first > grid[0]) {
    showMessage(MsgLevel::MSG_WARN, stringify("The lower extrapolation limit (", extrapolation_limits.first,") is within the set of grid values. Setting to smallest grid value (", grid[0],")."));
    extrapolation_limits.first = grid[0];
  }
  if (extrapolation_limits.second < grid.back()) {
    showMessage(MsgLevel::MSG_WARN, stringify("The upper extrapolation limit (", extrapolation_limits.first,") is within the set of grid values. Setting to largest grid value (", grid.back(),")."));
    extrapolation_limits.second = grid.back();
  }
}

GriddedData::GriddedData() = default;

GriddedData::GriddedData(std::vector<std::vector<double>> grid,
                         std::vector<std::vector<double>> values)
    : ndims(grid.size()), dimension_lengths(ndims), dimension_step_size(ndims), temp_coords(ndims) {
  construct_axes(grid);
  set_dimension_sizes();
  num_tables = values.size();
  results.resize(num_tables);

  value_tables = values;
}

GriddedData::GriddedData(std::vector<GridAxis> grid_axes, std::vector<std::vector<double>> values)
    : grid_axes(grid_axes),
      ndims(grid_axes.size()),
      dimension_lengths(ndims),
      dimension_step_size(ndims),
      temp_coords(ndims) {
  set_dimension_sizes();
  num_tables = values.size();
  results.resize(num_tables);

  value_tables = values;
}

GriddedData::GriddedData(std::vector<GridAxis> grid_axes)
    : grid_axes(grid_axes),
      ndims(grid_axes.size()),
      dimension_lengths(ndims),
      dimension_step_size(ndims),
      temp_coords(ndims) {
  set_dimension_sizes();
  num_tables = 0;
}

void GriddedData::set_dimension_sizes() {
  num_values = 1;
  for (std::size_t dim = ndims - 1; /* dim >= 0 */ dim < ndims; --dim) {
    std::size_t length = grid_axes[dim].get_length();
    dimension_lengths[dim] = length;
    dimension_step_size[dim] = num_values;
    num_values *= length;
  }
}

void GriddedData::construct_axes(const std::vector<std::vector<double>> &grid) {
  for (const auto &axis : grid) {
    grid_axes.emplace_back(axis);
  }
}

std::size_t GriddedData::add_value_table(std::vector<double> &value_vector) {
  if (value_vector.size() != num_values) {
    showMessage(MsgLevel::MSG_ERR, stringify("Input value table does not match the grid size: ",
                                             value_vector.size(), " != ", num_values));
  }
  value_tables.push_back(value_vector);
  num_tables++;
  results.resize(num_tables);
  return num_tables - 1;
}

std::size_t GriddedData::get_ndims() { return grid_axes.size(); }

std::size_t GriddedData::get_num_tables() { return num_tables; }

std::size_t GriddedData::get_value_index(const std::vector<std::size_t> &coords) {
  std::size_t index = 0;
  for (std::size_t dim = 0; dim < ndims; ++dim) {
    index += coords[dim] * dimension_step_size[dim];
  }
  return index;
}

std::size_t GriddedData::get_value_index_relative(const std::vector<std::size_t> &coords,
                                     const std::vector<short> &translation) {
  int new_coord;
  for (std::size_t dim = 0; dim < coords.size(); dim++) {
    new_coord = coords[dim] + translation[dim];
    if (new_coord < 0) {
      temp_coords[dim] = 0u;
    } else if (new_coord >= (int)dimension_lengths[dim]) {
      temp_coords[dim] = dimension_lengths[dim] - 1u;
    } else {
      temp_coords[dim] = new_coord;
    }
  }
  return get_value_index(temp_coords);
}

std::vector<double> GriddedData::get_values(const std::vector<std::size_t> &coords) {
  std::size_t index = get_value_index(coords);
  for (std::size_t i = 0; i < num_tables; ++i) {
    results[i] = value_tables[i][index];
  }
  return results;
}

std::vector<double> GriddedData::get_values(const std::size_t index) {
  for (std::size_t i = 0; i < num_tables; ++i) {
    results[i] = value_tables[i][index];
  }
  return results;
}

std::vector<double> GriddedData::get_values_relative(const std::vector<std::size_t> &coords,
                                                      const std::vector<short> &translation) {
  return get_values(get_value_index_relative(coords, translation));
}

const std::vector<double> &GriddedData::get_grid_vector(const std::size_t &dim) {
  return grid_axes[dim].grid;
}

std::pair<double, double> GriddedData::get_extrap_limits(const std::size_t &dim) {
  return grid_axes[dim].extrapolation_limits;
}

double GriddedData::get_axis_spacing_mult(const std::size_t &dim, const std::size_t &flavor,
                                          const std::size_t &index) {
  if (grid_axes[dim].interpolation_method == Method::CUBIC) {
    return grid_axes[dim].get_spacing_multiplier(flavor, index);
  } else {
    return 0.0;
  }
}

void GriddedData::set_axis_extrap_method(const std::size_t &dim,
                                         const Method extrapolation_method) {
  grid_axes[dim].extrapolation_method = extrapolation_method;
}

std::vector<Method> GriddedData::get_extrap_methods() {
  std::vector<Method> extrap_methods(ndims);
  for (std::size_t dim = 0; dim < ndims; dim++) {
    extrap_methods[dim] = grid_axes[dim].extrapolation_method;
  }
  return extrap_methods;
}

void GriddedData::set_axis_extrap_limits(const std::size_t &dim,
                                         const std::pair<double, double> &extrap_limits) {
  grid_axes[dim].set_extrap_limits(extrap_limits);
}

std::vector<Method> GriddedData::get_interp_methods() {
  std::vector<Method> interp_methods(ndims);
  for (std::size_t dim = 0; dim < ndims; dim++) {
    interp_methods[dim] = grid_axes[dim].interpolation_method;
  }
  return interp_methods;
}

void GriddedData::set_axis_interp_method(const std::size_t &dim,
                                         const Method interpolation_method) {
  grid_axes[dim].set_interp_method(interpolation_method);
}

void GriddedData::normalize_value_table(std::size_t table_num, double scalar) {
  auto &table = value_tables[table_num];
  scalar = 1.0/scalar;
  std::transform(table.begin(), table.end(), table.begin(),
                 std::bind(std::multiplies<double>(), std::placeholders::_1, scalar));
}

// free functions
bool free_check_sorted(std::vector<double> my_vec) {
  // ensures that the grid vector is strictly ascending
  auto first = my_vec.begin();
  auto last = my_vec.end();
  if (first == last)
    return true;

  auto next = first;
  while (++next != last) {
    if (*next <= *first)
      return false;
    ++first;
  }
  return true;
}

} // namespace Btwxt
