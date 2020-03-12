/* Copyright (c) 2018 Big Ladder Software LLC. All rights reserved.
 * See the LICENSE file for additional terms and conditions. */

// Standard
#include <iostream>
#include <numeric>
#include <algorithm>

// btwxt
#include "error.h"

namespace Btwxt {

GridPoint::GridPoint() {}

GridPoint::GridPoint(GriddedData &grid_data_in)
    : grid_data(&grid_data_in),
      ndims(grid_data->get_ndims()),
      target(ndims, 0.0),
      target_is_set(false),
      point_floor(ndims, 0),
      floor_index(0),
      weights(ndims, 0),
      is_inbounds(ndims),
      methods(ndims, Method::UNDEF),
      reset_hypercube(false),
      weighting_factors(ndims, std::vector<double>(4, 0.0)),
      interp_coeffs(ndims, std::vector<double>(2, 0.0)),
      cubic_slope_coeffs(ndims, std::vector<double>(2, 0.0)),
      results(grid_data->num_tables),
      hypercube_size_hash(0) {
}

GridPoint::GridPoint(GriddedData &grid_data_in, std::vector<double> v)
    : grid_data(&grid_data_in),
      ndims(grid_data->get_ndims()),
      target_is_set(false),
      point_floor(ndims, 0),
      weights(ndims, 0),
      is_inbounds(ndims),
      methods(ndims, Method::UNDEF),
      reset_hypercube(false),
      weighting_factors(ndims, std::vector<double>(4, 0.0)),
      interp_coeffs(ndims, std::vector<double>(2, 0.0)),
      cubic_slope_coeffs(ndims, std::vector<double>(2, 0.0)),
      results(grid_data->num_tables) {
  set_target(v);
}

void GridPoint::set_target(const std::vector<double> &v) {
  if (v.size() != ndims) {
    showMessage(MsgLevel::MSG_ERR,
                stringify("Target and Gridded Data do not have the same dimensions."));
  }
  if (target_is_set) {
    if (std::equal(v.begin(), v.end(), target.begin())) {
      return;
    }
  }
  target = v;
  target_is_set = true;
  set_floor();
  calculate_weights();
  consolidate_methods();
  calculate_interp_coeffs();
  set_results();
}

std::vector<double> GridPoint::get_current_target() {
  if (!target_is_set) {
    showMessage(MsgLevel::MSG_WARN,
                stringify("The current target was requested, but no target has been set."));
    return target;
  }
  return target;
}

std::vector<std::size_t> GridPoint::get_floor() { return point_floor; }

std::vector<double> GridPoint::get_weights() { return weights; }

std::vector<Bounds> GridPoint::get_is_inbounds() { return is_inbounds; }

std::vector<Method> GridPoint::get_methods() { return methods; }

std::vector<std::vector<double>> GridPoint::get_interp_coeffs() { return interp_coeffs; }

std::vector<std::vector<double>> GridPoint::get_cubic_slope_coeffs() { return cubic_slope_coeffs; }

void GridPoint::set_floor() {
  for (std::size_t dim = 0; dim < ndims; dim += 1) {
    set_dim_floor(dim);
  }
  floor_index = grid_data->get_value_index(point_floor);
}

void GridPoint::set_dim_floor(std::size_t dim) {
  GridAxis &axis = grid_data->grid_axes[dim];
  std::size_t l = axis.grid.size();
  if (target[dim] < axis.extrapolation_limits.first) {
    is_inbounds[dim] = Bounds::OUTLAW;
    point_floor[dim] = 0u;
  } else if (target[dim] > axis.extrapolation_limits.second) {
    is_inbounds[dim] = Bounds::OUTLAW;
    point_floor[dim] = std::max((int)l - 2, 0); // l-2 because that's the left side of the (l-2, l-1) edge.
  } else if (target[dim] < axis.grid[0]) {
    is_inbounds[dim] = Bounds::OUTBOUNDS;
    point_floor[dim] = 0;
  } else if (target[dim] > axis.grid.back()) {
    is_inbounds[dim] = Bounds::OUTBOUNDS;
    point_floor[dim] = std::max((int)l - 2, 0); // l-2 because that's the left side of the (l-2, l-1) edge.
  } else if (target[dim] == axis.grid.back()) {
    is_inbounds[dim] = Bounds::INBOUNDS;
    point_floor[dim] = std::max((int)l - 2, 0); // l-2 because that's the left side of the (l-2, l-1) edge.
  } else {
    is_inbounds[dim] = Bounds::INBOUNDS;
    std::vector<double>::const_iterator upper =
        std::upper_bound(axis.grid.begin(), axis.grid.end(), target[dim]);
    point_floor[dim] = upper - axis.grid.begin() - 1;
  }
}

void GridPoint::calculate_weights() {
  for (std::size_t dim = 0; dim < ndims; ++dim) {
    if (grid_data->grid_axes[dim].grid.size() > 1) {
      double edge[] = {grid_data->grid_axes[dim].grid[point_floor[dim]],
                       grid_data->grid_axes[dim].grid[point_floor[dim] + 1]};
      weights[dim] = compute_fraction(target[dim], edge);
    } else {
      weights[dim] = 1.0;
    }
  }
}

double compute_fraction(double x, double edge[2]) {
  // how far along an edge is the target?
  return (x - edge[0]) / (edge[1] - edge[0]);
}

void GridPoint::consolidate_methods()
// If out of bounds, extrapolate according to prescription
// If outside of extrapolation limits, send a warning and perform constant extrapolation.
{
  previous_methods = methods;
  methods = grid_data->get_interp_methods();
  if (target_is_set) {
    auto extrap_methods = grid_data->get_extrap_methods();
    for (std::size_t dim = 0; dim < ndims; dim++) {
      if (is_inbounds[dim]==Bounds::OUTBOUNDS) {
        methods[dim] = extrap_methods[dim];
      } else if (is_inbounds[dim]==Bounds::OUTLAW) {
        // showMessage(MsgLevel::MSG_WARN, stringify("The target is outside the extrapolation limits
        // in dimension ", dim,
        //                                ". Will perform constant extrapolation."));
        methods[dim] = Method::CONSTANT;
      }
    }
  }
  reset_hypercube |= !std::equal(previous_methods.begin(), previous_methods.end(), methods.begin());
  if (reset_hypercube) {
    set_hypercube(methods);
  }
}

void GridPoint::set_hypercube() { set_hypercube(grid_data->get_interp_methods()); }

void GridPoint::set_hypercube(std::vector<Method> methods) {
  if (methods.size() != ndims) {
    showMessage(MsgLevel::MSG_ERR, stringify("Error setting hypercube. Methods vector does not "
                                             "have the correct number of dimensions."));
  }
  std::size_t previous_size = hypercube.size();
  std::vector<std::vector<int>> options(ndims, {0, 1});
  reset_hypercube = false;

  hypercube_size_hash = 0;
  std::size_t digit = 1;
  for (std::size_t dim = 0; dim < ndims; dim++) {
    if (target_is_set && weights[dim] == 0.0) {
      options[dim] = {0};
      reset_hypercube = true;
    } else if (methods[dim] == Method::CUBIC) {
      options[dim] = {-1, 0, 1, 2};
    }
    hypercube_size_hash += options[dim].size()*digit;
    digit *= 10;
  }
  hypercube = {{}};
  for (const auto &list : options) {
    std::vector<std::vector<short>> r;
    for (const auto &x : hypercube) {
      for (const auto item : list) {
        r.push_back(x);
        r.back().push_back(item);
      }
    }
    hypercube = std::move(r);
  }
  if (hypercube.size() != previous_size) {
    hypercube_values.resize(hypercube.size(), std::vector<double>(grid_data->num_tables));
    hypercube_weights.resize(hypercube.size());
  }
}

std::vector<std::vector<short>> &GridPoint::get_hypercube() {
  consolidate_methods();
  return hypercube;
}

void GridPoint::calculate_interp_coeffs() {
  for (std::size_t dim = 0; dim < ndims; dim++) {
    double mu = weights[dim];
    if (methods[dim] == Method::CUBIC) {
      interp_coeffs[dim][0] = 2 * mu * mu * mu - 3 * mu * mu + 1;
      interp_coeffs[dim][1] = -2 * mu * mu * mu + 3 * mu * mu;
      cubic_slope_coeffs[dim][0] = (mu * mu * mu - 2 * mu * mu + mu)*grid_data->get_axis_spacing_mult(dim, 0, point_floor[dim]);
      cubic_slope_coeffs[dim][1] = (mu * mu * mu - mu * mu)*grid_data->get_axis_spacing_mult(dim, 1, point_floor[dim]);
    } else {
      if (methods[dim] == Method::CONSTANT) {
        mu = mu < 0 ? 0 : 1;
      }
      interp_coeffs[dim][0] = 1 - mu;
      interp_coeffs[dim][1] = mu;
      cubic_slope_coeffs[dim][0] = 0.0;
      cubic_slope_coeffs[dim][1] = 0.0;
    }
    weighting_factors[dim][0] = -cubic_slope_coeffs[dim][0]; // point below floor (-1)
    weighting_factors[dim][1] = interp_coeffs[dim][0] - cubic_slope_coeffs[dim][1]; // floor (0)
    weighting_factors[dim][2] = interp_coeffs[dim][1] + cubic_slope_coeffs[dim][0]; // ceiling (1)
    weighting_factors[dim][3] = cubic_slope_coeffs[dim][1]; // point above ceiling (2)
  }
}

void GridPoint::set_hypercube_values() {
  if (results.size() != grid_data->num_tables) {
    results.resize(grid_data->num_tables);
    hypercube_values.resize(hypercube.size(), std::vector<double>(grid_data->num_tables));
    hypercube_cache.clear();
  }
  if (hypercube_cache.count({floor_index,hypercube_size_hash})) {
    hypercube_values = hypercube_cache.at({floor_index,hypercube_size_hash});
    return;
  }
  std::size_t hypercube_index = 0;
  for (const auto &v : hypercube) {
    hypercube_values[hypercube_index] = grid_data->get_values_relative(point_floor, v);
    ++hypercube_index;
  }
  hypercube_cache[{floor_index,hypercube_size_hash}] = hypercube_values;
}

void GridPoint::set_results() {
  set_hypercube_values();
  std::fill(results.begin(), results.end(), 0.0);
  for (std::size_t i = 0; i < hypercube.size(); ++i) {
    hypercube_weights[i] = get_vertex_weight(hypercube[i]);
    const auto &values = hypercube_values[i];
    for (std::size_t j = 0; j < grid_data->num_tables; ++j) {
      results[j] += values[j] * hypercube_weights[i];
    }
  }
}

std::vector<double> GridPoint::get_results() {
  if (grid_data->num_tables == 0u) {
    showMessage(MsgLevel::MSG_WARN,
                stringify("There are no value tables in the gridded data. No results returned."));
  }
  if (!target_is_set) {
    showMessage(MsgLevel::MSG_WARN,
                stringify("Results were requested, but no target has been set."));
  }
  return results;
}

double GridPoint::get_vertex_weight(const std::vector<short> &v) {
  double weight = 1.0;
  for (std::size_t dim = 0; dim < ndims; dim++) {
    weight *= weighting_factors[dim][v[dim] + 1];
  }
  return weight;
}

void GridPoint::normalize_grid_values_at_target(const double scalar) {
  if (!target_is_set) {
    showMessage(MsgLevel::MSG_WARN,
                stringify("Cannot normalize grid values. No target has been set."));
    return;
  }
  for (std::size_t table_index = 0; table_index < grid_data->num_tables; ++table_index) {
    grid_data->normalize_value_table(table_index,results[table_index]*scalar);
  }
  hypercube_cache.clear();
  set_results();
}

double GridPoint::normalize_grid_values_at_target(std::size_t table_num, const double scalar) {
  if (!target_is_set) {
    showMessage(MsgLevel::MSG_WARN,
                stringify("Cannot normalize grid values. No target has been set."));
    return scalar;
  }
  // create a scalar which represents the product of the inverted normalization factor and the value in the table at the independent variable reference value
  double total_scalar = results[table_num]*scalar;
  grid_data->normalize_value_table(table_num,total_scalar);
  hypercube_cache.clear();
  set_results();

  return total_scalar;
}

} // namespace Btwxt
