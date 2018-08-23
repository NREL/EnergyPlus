/* Copyright (c) 2018 Big Ladder Software LLC. All rights reserved.
* See the LICENSE file for additional terms and conditions. */

#ifndef GRIDINTERP_H_
#define GRIDINTERP_H_

// Standard
#include<vector>
#include "Eigen/Dense"

// btwxt
#include "griddeddata.h"
#include "gridpoint.h"
#include "hypercube.h"


namespace Btwxt {


// this will be the public-facing class.
    class RegularGridInterpolator {
    public:
        // GridSpace, GridAxis, AllValueTables, ValueTable are instantiated in RGI constructor.
        RegularGridInterpolator();

        explicit RegularGridInterpolator(GriddedData &the_blob);

        RegularGridInterpolator(
                const std::vector<std::vector<double> > &grid,
                const std::vector<std::vector<double> > &values
        );

        // Add value table to GriddedData
        std::size_t add_value_table(std::vector<double>& value_vector) {
            return grid_data.add_value_table(value_vector);
        }

        // GridPoint gets instantiated inside calculate_value_at_target
        double calculate_value_at_target(std::vector<double> target, std::size_t table_index);

        double operator()(std::vector<double> target, std::size_t table_index) {
            return calculate_value_at_target(std::move(target), table_index);
        }

        double calculate_value_at_target(std::size_t table_index);

        double operator()(std::size_t table_index) {
            return calculate_value_at_target(table_index);
        }

        std::vector<double> calculate_all_values_at_target(std::vector<double> target);

        std::vector<double> operator()(std::vector<double> target) {
            return calculate_all_values_at_target(std::move(target));
        }

        std::vector<double> calculate_all_values_at_target();

        std::vector<double> operator()() {
            return calculate_all_values_at_target();
        }

        void set_new_grid_point(const std::vector<double> &target);

        std::vector<double> get_current_grid_point();

        void clear_current_grid_point();

        std::size_t get_ndims();

        std::pair<double, double> get_axis_limits(int dim);

    private:
        GriddedData grid_data;
        bool cgp_exists;
        GridPoint current_grid_point;
        PointLocator the_locator;
        Hypercube hypercube;

        void check_target_dimensions(const std::vector<double> &target);

        std::vector<double> interpolation_wrapper();
    };


// free functions
    std::size_t pow(const std::size_t &base, const std::size_t &power);
}
#endif // GRIDINTERP_H_
