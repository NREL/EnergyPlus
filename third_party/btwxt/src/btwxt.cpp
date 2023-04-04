/* Copyright (c) 2018 Big Ladder Software LLC. All rights reserved.
 * See the LICENSE file for additional terms and conditions. */

// Standard
#include <iostream>
#include <numeric>

// btwxt
#include "btwxt.h"
#include "error.h"

namespace Btwxt {

RegularGridInterpolator::RegularGridInterpolator(const std::vector<std::vector<double>> &grid)
    : grid_data(grid), grid_point(grid_data) {}

RegularGridInterpolator::RegularGridInterpolator(const std::vector<std::vector<double>> &grid,
                                                 const std::vector<std::vector<double>> &values)
    : grid_data(grid, values), grid_point(grid_data) {}

RegularGridInterpolator::RegularGridInterpolator(const RegularGridInterpolator &source)
    : grid_data(source.grid_data), grid_point(source.grid_point) {
  *this = source;
}

std::size_t RegularGridInterpolator::add_value_table(const std::vector<double> &value_vector) {
  try {
    return grid_data.add_value_table(value_vector);
  } catch (BtwxtErr &e) {
    log_message(MsgLevel::MSG_ERR, e.what());
    return 0;
  }
}

double RegularGridInterpolator::get_value_at_target(const std::vector<double>& target,
                                                    std::size_t table_index) {
  set_new_target(target);
  auto results = grid_point.get_results();
  return results[table_index];
}

double RegularGridInterpolator::get_value_at_target(std::size_t table_index) {
  auto results = grid_point.get_results();
  return results[table_index];
}

std::vector<double>
RegularGridInterpolator::get_values_at_target(const std::vector<double> &target) {
  set_new_target(target);
  return grid_point.get_results();
}

std::vector<double> RegularGridInterpolator::get_values_at_target() {
  return grid_point.get_results();
}

double RegularGridInterpolator::normalize_values_at_target(std::size_t table_index,
                                                           const std::vector<double> &target,
                                                           const double scalar) {
  set_new_target(target);
  return normalize_values_at_target(table_index, scalar);
}

double RegularGridInterpolator::normalize_values_at_target(std::size_t table_index,
                                                           const double scalar) {
  try {
    return grid_point.normalize_grid_values_at_target(table_index, scalar);
  } catch (BtwxtErr &e) {
    log_message(MsgLevel::MSG_ERR, e.what());
    return scalar; // TODO:
  }
}

void RegularGridInterpolator::normalize_values_at_target(const std::vector<double> &target,
                                                         const double scalar) {
  set_new_target(target);
  normalize_values_at_target(scalar);
}

void RegularGridInterpolator::normalize_values_at_target(const double scalar) {
  try {
    return grid_point.normalize_grid_values_at_target(scalar);
  } catch (BtwxtErr &e) {
    log_message(MsgLevel::MSG_ERR, e.what());
  }
}

void RegularGridInterpolator::set_new_target(const std::vector<double> &target) {
  try {
    grid_point.set_target(target);
  } catch (BtwxtErr &e) {
    log_message(MsgLevel::MSG_ERR, e.what());
  }
}

std::vector<double> RegularGridInterpolator::get_current_target() const {
  return grid_point.get_current_target();
}

void RegularGridInterpolator::clear_current_target() {
  grid_point = GridPoint(grid_data, &callback_function_, caller_context_);
}

std::size_t RegularGridInterpolator::get_ndims() const { return grid_data.get_ndims(); }

std::size_t RegularGridInterpolator::get_num_tables() const { return grid_data.get_num_tables(); }

std::pair<double, double> RegularGridInterpolator::get_axis_limits(int dim) {
  return grid_data.get_extrap_limits(dim);
}

void RegularGridInterpolator::set_logging_callback(BtwxtLoggerFn callback_function,
                                                   void *caller_info) {
  callback_function_ = callback_function;
  caller_context_ = caller_info;
  grid_data.set_logging_callback(&callback_function_, caller_context_);
  grid_point.set_logging_callback(&callback_function_, caller_context_);
}

void RegularGridInterpolator::log_message(MsgLevel messageType, std::string_view message) const {
  if (callback_function_) {
    callback_function_(messageType, message, caller_context_);
  } else if (btwxtCallbackFunction) {
    showMessage(messageType, std::string{message});
  }
}

} // namespace Btwxt
