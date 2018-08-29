/* Copyright (c) 2018 Big Ladder Software LLC. All rights reserved.
 * See the LICENSE file for additional terms and conditions. */

// Standard
#include <iostream>
#include <numeric>

// btwxt
#include "error.h"

namespace Btwxt {

std::vector<std::pair<int, int>> GridPoint::sivor = {{-1, 0}, {-1, 1}, {1, 0}, {1, 1}};

GridPoint::GridPoint() {}

GridPoint::GridPoint(GriddedData &grid_data_in)
    : grid_data(&grid_data_in), ndims(grid_data->get_ndims()), target(ndims, 0.0),
      target_is_set(false), point_floor(ndims, 0), weights(ndims, 0), is_inbounds(ndims),
      methods(ndims, Method::UNDEF), interp_coeffs(ndims, std::vector<double>(2, 0.0)),
      cubic_slope_coeffs(ndims, std::vector<double>(2, 0.0)),
      terms(ndims, std::vector<double>(2, 0.0)), results(grid_data->num_tables) {}

GridPoint::GridPoint(GriddedData &grid_data_in, std::vector<double> v)
    : grid_data(&grid_data_in), ndims(grid_data->get_ndims()), point_floor(ndims, 0),
      weights(ndims, 0), is_inbounds(ndims), methods(ndims, Method::UNDEF),
      interp_coeffs(ndims, std::vector<double>(2, 0.0)),
      cubic_slope_coeffs(ndims, std::vector<double>(2, 0.0)),
      terms(ndims, std::vector<double>(2, 0.0)), results(grid_data->num_tables) {
  set_target(v);
}

void GridPoint::set_target(std::vector<double> v) {
  if (v.size() != ndims) {
    showMessage(MsgLevel::MSG_ERR,
                stringify("Target and Gridded Data do not have the same dimensions."));
  }
  target = v;
  target_is_set = true;
  set_floor();
  calculate_weights();
  consolidate_methods();
  calculate_interp_coeffs();
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
}

void GridPoint::set_dim_floor(std::size_t dim) {
  GridAxis &axis = grid_data->grid_axes[dim];
  std::size_t l = axis.grid.size();
  if (target[dim] < axis.extrapolation_limits.first) {
    is_inbounds[dim] = Bounds::OUTLAW;
    point_floor[dim] = 0u;
  } else if (target[dim] > axis.extrapolation_limits.second) {
    is_inbounds[dim] = Bounds::OUTLAW;
    point_floor[dim] = l - 2; // l-2 because that's the left side of the (l-2, l-1) edge.
  } else if (target[dim] < axis.grid[0]) {
    is_inbounds[dim] = Bounds::OUTBOUNDS;
    point_floor[dim] = 0;
  } else if (target[dim] > axis.grid.back()) {
    is_inbounds[dim] = Bounds::OUTBOUNDS;
    point_floor[dim] = l - 2; // l-2 because that's the left side of the (l-2, l-1) edge.
  } else if (target[dim] == axis.grid.back()) {
    is_inbounds[dim] = Bounds::INBOUNDS;
    point_floor[dim] = l - 2; // l-2 because that's the left side of the (l-2, l-1) edge.
  } else {
    is_inbounds[dim] = Bounds::INBOUNDS;
    std::vector<double>::const_iterator upper =
        std::upper_bound(axis.grid.begin(), axis.grid.end(), target[dim]);
    point_floor[dim] = upper - axis.grid.begin() - 1;
  }
}

void GridPoint::calculate_weights() {
  for (std::size_t dim = 0; dim < ndims; dim += 1) {
    double edge[] = {grid_data->grid_axes[dim].grid[point_floor[dim]],
                     grid_data->grid_axes[dim].grid[point_floor[dim] + 1]};
    weights[dim] = compute_fraction(target[dim], edge);
  }
}

void GridPoint::set_hypercube() { set_hypercube(grid_data->get_interp_methods()); }

void GridPoint::set_hypercube(std::vector<Method> methods) {
  if (methods.size() != ndims) {
    showMessage(MsgLevel::MSG_ERR, stringify("Error setting hypercube. Methods vector does not "
                                             "have the correct number of dimensions."));
  }
  std::vector<std::vector<int>> options(ndims, {0, 1});

  for (std::size_t dim = 0; dim < ndims; dim++) {
    if (methods[dim] == Method::CUBIC) {
      options[dim] = {-1, 0, 1, 2};
    }
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
}

std::vector<std::vector<short>> &GridPoint::get_hypercube() {
  consolidate_methods();
  return hypercube;
}

void GridPoint::consolidate_methods()
// If out of bounds, extrapolate according to prescription
// If outside of extrapolation limits, send a warning and perform constant extrapolation.
{
  std::vector<Method> previous_methods = methods;
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
  if (!std::equal(previous_methods.begin(), previous_methods.end(), methods.begin())) {
    set_hypercube(methods);
  }
}

void GridPoint::calculate_interp_coeffs() {
  for (std::size_t dim = 0; dim < ndims; dim++) {
    double mu = weights[dim];
    if (methods[dim] == Method::CUBIC) {
      interp_coeffs[dim][0] = 2 * mu * mu * mu - 3 * mu * mu + 1;
      interp_coeffs[dim][1] = -2 * mu * mu * mu + 3 * mu * mu;
      cubic_slope_coeffs[dim][0] = mu * mu * mu - 2 * mu * mu + mu;
      cubic_slope_coeffs[dim][1] = mu * mu * mu - mu * mu;
    } else if (methods[dim] == Method::CONSTANT) {
      mu = mu < 0 ? 0 : 1;
      interp_coeffs[dim][0] = 1 - mu;
      interp_coeffs[dim][1] = mu;
    } else { // LINEAR
      interp_coeffs[dim][0] = 1 - mu;
      interp_coeffs[dim][1] = mu;
    }
  }
}

std::vector<double> GridPoint::get_results() {
  if (results.size() != grid_data->num_tables) {
    results.resize(grid_data->num_tables);
  }
  if (results.size() == 0u) {
    showMessage(MsgLevel::MSG_WARN,
                stringify("There are no value tables in the gridded data. No results returned."));
    return results;
  }
  std::fill(results.begin(), results.end(), 0.0);
  if (!target_is_set) {
    showMessage(MsgLevel::MSG_WARN,
                stringify("Results were requested, but no target has been set."));
    return results;
  }
  double weight;
  std::vector<std::vector<double>> spacing_mults = get_spacing_mults(*grid_data);
  for (const auto &v : hypercube) {
    weight = get_vertex_weight(v, spacing_mults);
    std::vector<double> values = grid_data->get_column_near_safe(point_floor, v);
    for (std::size_t i = 0; i < grid_data->num_tables; ++i) {
      results[i] += values[i] * weight;
    }
  }
  return results;
}

std::vector<std::vector<double>> GridPoint::get_spacing_mults(GriddedData &grid_data) {
  std::vector<std::vector<double>> spacing_mults(ndims, std::vector<double>(2, 0));
  for (std::size_t dim = 0; dim < ndims; dim++) {
    if (methods[dim] == Method::CUBIC) {
      spacing_mults[dim][0] = grid_data.get_axis_spacing_mult(dim, 0, point_floor[dim]);
      spacing_mults[dim][1] = grid_data.get_axis_spacing_mult(dim, 1, point_floor[dim]);
    }
  }
  return spacing_mults;
}

double GridPoint::get_vertex_weight(const std::vector<short> &v,
                                    const std::vector<std::vector<double>> &spacing_mults) {
  int sign, flavor;
  for (std::size_t dim = 0; dim < ndims; dim++) {
    if (methods[dim] == Method::CUBIC) {
      std::tie(sign, flavor) = sivor[v[dim] + 1];
      if (v[dim] == 0 || v[dim] == 1) { // closest neighbors have a normal interpolation term
        terms[dim][0] = interp_coeffs[dim][v[dim]];
      } else {
        terms[dim][0] = 0.0;
      }
      terms[dim][1] = cubic_slope_coeffs[dim][flavor] * spacing_mults[dim][flavor] * sign;
    } else { // LINEAR or CONSTANT
      terms[dim][0] = interp_coeffs[dim][v[dim]];
      terms[dim][1] = 0.0;
    }
  }
  return sum_weighting_terms();
}

double GridPoint::sum_weighting_terms() {
  std::size_t nT = terms.size();
  std::size_t N = 1;
  for (std::size_t dim = 0; dim < nT; ++dim) {
    N *= 2;
  } // Two options per dimension (normal or slope terms)

  // Add all combinations of coefficient term products
  double weight_sum = 0.0;
  for (std::size_t n = 0; n < N; ++n) {
    double product = 1.0;
    for (std::size_t dim = 0; dim < nT; ++dim) {
      product *= terms[dim][bool(n & 1 << dim)];
    }
    weight_sum += product;
  }
  return weight_sum;
}

double compute_fraction(double x, double edge[2]) {
  // how far along an edge is the target?
  return (x - edge[0]) / (edge[1] - edge[0]);
}

} // namespace Btwxt
