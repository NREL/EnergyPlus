/* Copyright (c) 2018 Big Ladder Software LLC. All rights reserved.
 * See the LICENSE file for additional terms and conditions. */

#ifndef GRIDINTERP_H_
#define GRIDINTERP_H_

// Standard
#include <functional>
#include <vector>

// btwxt
#include "error.h"
#include "griddeddata.h"
#include "gridpoint.h"

namespace Btwxt {

// this will be the public-facing class.
class RegularGridInterpolator {
public:
  // GridSpace, GridAxis, AllValueTables, ValueTable are instantiated in RGI constructor.
  RegularGridInterpolator() = default;

  RegularGridInterpolator(const std::vector<std::vector<double>> &grid);

  RegularGridInterpolator(const std::vector<std::vector<double>> &grid,
                          const std::vector<std::vector<double>> &values);

  RegularGridInterpolator(const RegularGridInterpolator &source);

  RegularGridInterpolator &operator=(const RegularGridInterpolator &source) {
    if (this == &source) {
      return *this;
    }

    grid_data = source.grid_data;
    grid_point = source.grid_point;
    if (source.grid_point.grid_data != nullptr) {
      this->grid_point.grid_data = &this->grid_data;
    }
    return *this;
  }

  // Add value table to GriddedData
  std::size_t add_value_table(const std::vector<double> &value_vector);

  // GridPoint gets instantiated inside calculate_value_at_target
  double get_value_at_target(const std::vector<double>& target, std::size_t table_index);

  double operator()(std::vector<double> target, std::size_t table_index) {
    return get_value_at_target(std::move(target), table_index);
  }

  double get_value_at_target(std::size_t table_index);

  double operator()(std::size_t table_index) { return get_value_at_target(table_index); }

  std::vector<double> get_values_at_target();

  std::vector<double> get_values_at_target(const std::vector<double> &target);

  std::vector<double> operator()(const std::vector<double> &target) {
    return get_values_at_target(target);
  }

  std::vector<double> operator()() { return get_values_at_target(); }

  void set_new_target(const std::vector<double> &target);

  void normalize_values_at_target(const double scalar = 1.0);

  void normalize_values_at_target(const std::vector<double> &target, const double scalar = 1.0);

  double normalize_values_at_target(std::size_t table_index, const double scalar = 1.0);

  double normalize_values_at_target(std::size_t table_index, const std::vector<double> &target,
                                    const double scalar = 1.0);

  std::vector<double> get_current_target() const;

  void clear_current_target();

  std::size_t get_ndims() const;

  std::size_t get_num_tables() const;

  void set_axis_interp_method(std::size_t dim, Method method) {
    grid_data.set_axis_interp_method(dim, method);
  }

  void set_axis_extrap_method(const std::size_t &dim, Method method) {
    grid_data.set_axis_extrap_method(dim, method);
  }

  void set_axis_extrap_limits(const std::size_t &dim,
                              const std::pair<double, double> &extrap_limits) {
    grid_data.set_axis_extrap_limits(dim, extrap_limits);
  }

  std::pair<double, double> get_axis_limits(int dim);

  BtwxtLoggerFn callback_function_;
  void *caller_context_;

  void set_logging_callback(BtwxtLoggerFn callback_function, void *caller_info);

  void log_message(MsgLevel messageType, std::string_view message) const;

private:
  GriddedData grid_data;
  GridPoint grid_point;
};

} // namespace Btwxt
#endif // GRIDINTERP_H_
