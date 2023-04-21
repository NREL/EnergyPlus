/* Copyright (c) 2018 Big Ladder Software LLC. All rights reserved.
 * See the LICENSE file for additional terms and conditions. */

#ifndef GRIDINTERP_H_
#define GRIDINTERP_H_

// Standard
#include <vector>

// btwxt
#include "griddeddata.h"
#include "gridpoint.h"

namespace Btwxt {

enum class MsgLevel { MSG_DEBUG, MSG_INFO, MSG_WARN, MSG_ERR };
extern int LOG_LEVEL;

typedef void (*BtwxtCallbackFunction)(const MsgLevel messageType, const std::string message,
                                      void *contextPtr);

extern BtwxtCallbackFunction btwxtCallbackFunction;
extern void *messageCallbackContextPtr;

void setMessageCallback(BtwxtCallbackFunction callbackFunction, void *contextPtr);

// this will be the public-facing class.
class RegularGridInterpolator {
public:
  // GridSpace, GridAxis, AllValueTables, ValueTable are instantiated in RGI constructor.

  RegularGridInterpolator();

  explicit RegularGridInterpolator(GriddedData &grid_data);

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
  std::size_t add_value_table(std::vector<double> &value_vector) {
    return grid_data.add_value_table(value_vector);
  }

  // GridPoint gets instantiated inside calculate_value_at_target
  double get_value_at_target(std::vector<double> target, std::size_t table_index);

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

  double normalize_values_at_target(std::size_t table_index, const std::vector<double> &target, const double scalar = 1.0);

  std::vector<double> get_current_target();

  void clear_current_target();

  std::size_t get_ndims();

  void set_axis_interp_method(std::size_t dim, Method method) {
    grid_data.set_axis_interp_method(dim, method);
  }

  std::vector<std::vector<short>>& get_hypercube();

  std::pair<double, double> get_axis_limits(int dim);

private:
  GriddedData grid_data;
  GridPoint grid_point;
};

} // namespace Btwxt
#endif // GRIDINTERP_H_
