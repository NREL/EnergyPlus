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
    : grid(std::move(grid_vector)), extrapolation_method(extrapolation_method),
      interpolation_method(interpolation_method),
      extrapolation_limits(std::move(extrapolation_limits)) {
  check_grid_sorted();
  check_extrap_limits();
  if (interpolation_method == Method::CUBIC) {
    spacing_multiplier = calc_spacing_multipliers();
  }

  showMessage(MsgLevel::MSG_DEBUG, "GridAxis object constructed from vector!");
}

std::size_t GridAxis::get_length() { return grid.size(); }

void GridAxis::set_interp_method(const Method im) {
  interpolation_method = im;
  if (im == Method::CUBIC) {
    spacing_multiplier = calc_spacing_multipliers();
  }
}

void GridAxis::set_extrap_method(const Method em) { extrapolation_method = em; }

void GridAxis::set_extrap_limits(const std::pair<double, double> extrap_limits) {
  extrapolation_limits = extrap_limits;
  check_extrap_limits();
}

double GridAxis::get_spacing_multiplier(const std::size_t &flavor, const std::size_t &index) {
  return spacing_multiplier[flavor][index];
}

std::vector<std::vector<double>> GridAxis::calc_spacing_multipliers() {
  std::size_t grid_size = grid.size();
  std::vector<std::vector<double>> v(2, std::vector<double>(grid_size - 1, 1.0));
  // "0" and "1" are the "flavors" of the calc_spacing_multipliers.
  // If you are sitting at the "0" along an edge of they hypercube, you want the "0" flavof
  double center_spacing;
  for (std::size_t i = 0; i < grid.size() - 1; i++) {
    center_spacing = grid[i + 1] - grid[i];
    if (i != 0) {
      v[0][i] = center_spacing / (grid[i + 1] - grid[i - 1]);
    }
    if (i + 2 != grid_size) {
      v[1][i] = center_spacing / (grid[i + 2] - grid[i]);
    }
  }
  return v;
}

void GridAxis::check_grid_sorted() {
  bool grid_is_sorted = Btwxt::free_check_sorted(grid);
  if (!grid_is_sorted) {
    showMessage(MsgLevel::MSG_ERR, "axis is not sorted.");
  }
}

void GridAxis::check_extrap_limits() {
  if (extrapolation_limits.first > grid[0]) {
    showMessage(MsgLevel::MSG_WARN, "The lower extrapolation limit is within the grid. "
                                    "Setting to smallest value.");
    extrapolation_limits.first = grid[0];
  }
  if (extrapolation_limits.second < grid.back()) {
    showMessage(MsgLevel::MSG_WARN, "The upper extrapolation limit is within the grid. "
                                    "Setting to largest value.");
    extrapolation_limits.second = grid.back();
  }
}

GriddedData::GriddedData() = default;

GriddedData::GriddedData(std::vector<std::vector<double>> grid,
                         std::vector<std::vector<double>> values) {
  ndims = grid.size();
  num_values = 1;
  for (const auto &grid_vector : grid) {
    num_values *= grid_vector.size();
    dimension_lengths.push_back(grid_vector.size());
  }
  num_tables = values.size();

  construct_axes(grid);
  value_tables = values;
}

GriddedData::GriddedData(std::vector<GridAxis> grid_axes, std::vector<std::vector<double>> values)
    : grid_axes(grid_axes), ndims(grid_axes.size()) {
  num_values = 1;
  for (auto grid_vector : grid_axes) {
    num_values *= grid_vector.get_length();
    dimension_lengths.push_back(grid_vector.get_length());
  }
  num_tables = values.size();

  value_tables = values;
}

GriddedData::GriddedData(std::vector<GridAxis> grid_axes)
    : grid_axes(grid_axes), ndims(grid_axes.size()) {
  num_values = 1;
  for (auto grid_vector : grid_axes) {
    num_values *= grid_vector.get_length();
    dimension_lengths.push_back(grid_vector.get_length());
  }
  num_tables = 0;
}

void GriddedData::construct_axes(const std::vector<std::vector<double>> &grid) {
  for (const auto &axis : grid) {
    GridAxis ga(axis);
    grid_axes.push_back(ga);
  }
}

std::size_t GriddedData::add_value_table(std::vector<double> &value_vector) {
  if (value_vector.size() != num_values) {
    showMessage(MsgLevel::MSG_ERR, stringify("Input value table does not match the grid size: ",
                                             value_vector.size(), " != ", num_values));
  }
  value_tables.push_back(value_vector);
  num_tables++;
  return num_tables - 1;
}

std::size_t GriddedData::get_ndims() { return grid_axes.size(); }

std::size_t GriddedData::get_num_tables() { return num_tables; }

std::size_t GriddedData::locate_coords(const std::vector<std::size_t> &coords) {
  std::size_t index = 0;
  std::size_t panel_size = 1;
  std::size_t ndims = dimension_lengths.size();
  for (std::size_t dim = ndims - 1; /* dim >= 0 */ dim < ndims; --dim) {
    if (coords[dim] >= dimension_lengths[dim]) {
      showMessage(MsgLevel::MSG_ERR, stringify("Overran dimension ", dim));
    }
    index += coords[dim] * panel_size;
    panel_size *= dimension_lengths[dim];
  }
  return index;
}

std::vector<double> GriddedData::get_column(const std::vector<std::size_t> &coords) {
  std::size_t index = locate_coords(coords);
  std::vector<double> result(num_tables);
  for (std::size_t i = 0; i < num_tables; ++i) {
    result[i] = value_tables[i][index];
  }
  return result;
}

std::vector<double> GriddedData::get_values(const std::vector<std::size_t> &coords) {
  return get_column(coords);
}

std::vector<double> GriddedData::get_column_near_safe(const std::vector<std::size_t> &coords,
                                                      const std::vector<short> &translation) {
  std::vector<std::size_t> result(translation.size());
  for (std::size_t dim = 0; dim < coords.size(); dim++) {
    int res = coords[dim] + translation[dim];
    if (res < 0) {
      result[dim] = 0;
    } else if (res >= (int)dimension_lengths[dim]) {
      result[dim] = dimension_lengths[dim] - 1;
    } else {
      result[dim] = res;
    }
  }
  return get_column(result);
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
