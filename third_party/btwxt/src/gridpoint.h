/* Copyright (c) 2018 Big Ladder Software LLC. All rights reserved.
 * See the LICENSE file for additional terms and conditions. */

#ifndef GRIDPOINT_H_
#define GRIDPOINT_H_

// Standard
#include <memory>
#include <vector>

// btwxt
#include "griddeddata.h"

namespace Btwxt {

enum class Bounds { OUTLAW, OUTBOUNDS, INBOUNDS };

class GridPoint {
public:
  GridPoint();

  GridPoint(GriddedData &grid_data);

  GridPoint(GriddedData &grid_data, std::vector<double> v);

  void set_target(const std::vector<double> &v);

  std::vector<double> get_current_target();

  std::vector<std::size_t> get_floor();

  std::vector<double> get_weights();

  std::vector<Bounds> get_is_inbounds();

  std::vector<Method> get_methods();

  std::vector<std::vector<double>> get_interp_coeffs();

  std::vector<std::vector<double>> get_cubic_slope_coeffs();

  std::vector<double> get_results();

  double get_vertex_weight(const std::vector<short> &v);

  void set_floor();

private:
  friend class RegularGridInterpolator;
  GriddedData *grid_data;
  std::size_t ndims;
  std::vector<double> target;
  bool target_is_set;
  std::vector<std::size_t> point_floor; // index of grid point <= target
  std::vector<double> weights;
  std::vector<Bounds> is_inbounds; // for deciding interpolation vs. extrapolation;
  std::vector<Method> methods;
  std::vector<Method> previous_methods;
  std::vector<std::vector<short>> hypercube;

  std::vector<std::vector<double>> interp_coeffs;
  std::vector<std::vector<double>> cubic_slope_coeffs;
  std::vector<std::vector<double>> spacing_mults;
  std::vector<std::vector<double>> terms;
  std::vector<double> temp_values;
  std::vector<double> results;
  static std::vector<std::pair<int, int>> sivor; // {sign, flavor}, flavor corresponds to the floor
                                                 // (0) or ceiling (1) bounding of the point

  void calculate_weights();

  void consolidate_methods();

  void calculate_interp_coeffs();

  double sum_weighting_terms();

  void set_dim_floor(std::size_t dim);

  void set_hypercube();

  void set_hypercube(std::vector<Method> methods);

  std::vector<std::vector<short>>& get_hypercube();


};

// free functions
double compute_fraction(double x, double edge[2]);
} // namespace Btwxt

#endif // GRIDPOINT_H_
