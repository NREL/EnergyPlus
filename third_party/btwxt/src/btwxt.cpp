/* Copyright (c) 2018 Big Ladder Software LLC. All rights reserved.
* See the LICENSE file for additional terms and conditions. */


// Standard
#include <iostream>
#include <numeric>

//btwxt
#include "btwxt.h"
#include "error.h"

namespace Btwxt {


    RegularGridInterpolator::RegularGridInterpolator() = default;;

    RegularGridInterpolator::RegularGridInterpolator(GriddedData &the_blob) :
            grid_data(the_blob),
            cgp_exists(false),
            current_grid_point()  // instantiates an empty GridPoint
    {
        std::size_t ndims{get_ndims()};
        hypercube = Hypercube(ndims, the_blob.get_interp_methods());
        showMessage(MsgLevel::MSG_DEBUG, "RGI constructed from GriddedData!");
    };

    RegularGridInterpolator::RegularGridInterpolator(
            const std::vector<std::vector<double> > &grid,
            const std::vector<std::vector<double> > &values
    ) :
            grid_data(grid, values),
            cgp_exists(false),
            current_grid_point()  // instantiates an empty GridPoint
    {
        std::size_t ndims{get_ndims()};
        hypercube = Hypercube(ndims, grid_data.get_interp_methods());
        showMessage(MsgLevel::MSG_DEBUG, "RGI constructed from vectors!");
    };

    double RegularGridInterpolator::calculate_value_at_target(
            std::vector<double> target, std::size_t table_index) {
        set_new_grid_point(target);
        std::vector<double> result = interpolation_wrapper();
        return result[table_index];
    };

    double RegularGridInterpolator::calculate_value_at_target(
            std::size_t table_index) {
        if (cgp_exists) {
            std::vector<double> result = interpolation_wrapper();
            return result[table_index];
        }
        showMessage(MsgLevel::MSG_WARN, "No target has been defined!");
        return 0;
    };

    std::vector<double> RegularGridInterpolator::calculate_all_values_at_target(
            std::vector<double> target) {
        set_new_grid_point(target);
        std::vector<double> result = interpolation_wrapper();
        return result;
    };

    std::vector<double> RegularGridInterpolator::calculate_all_values_at_target() {
        if (cgp_exists) {
            std::vector<double> result = interpolation_wrapper();
            return result;
        }
        showMessage(MsgLevel::MSG_WARN, "No target has been defined!");
        return {0};
    };

    void RegularGridInterpolator::set_new_grid_point(
            const std::vector<double> &target) {
        RegularGridInterpolator::check_target_dimensions(target);
        current_grid_point = GridPoint(target);
        cgp_exists = true;
        the_locator = PointLocator(current_grid_point, grid_data);
    };

    std::vector<double> RegularGridInterpolator::get_current_grid_point() {
        if (cgp_exists) {
            return current_grid_point.target;
        }
        showMessage(MsgLevel::MSG_WARN, "No target has been defined!");
        return {0};
    }

    void RegularGridInterpolator::clear_current_grid_point() {
        current_grid_point = GridPoint();
        cgp_exists = false;
        the_locator = PointLocator();
    };

    std::size_t RegularGridInterpolator::get_ndims() { return grid_data.get_ndims(); };

    std::pair<double, double> RegularGridInterpolator::get_axis_limits(int dim) {
        return grid_data.get_extrap_limits(dim);
    }

    void RegularGridInterpolator::check_target_dimensions(
            const std::vector<double> &target) {
        std::size_t ndims = grid_data.get_ndims();
        if (ndims == target.size()) {
            showMessage(MsgLevel::MSG_DEBUG, stringify(
                    "Target and GridSpace dimensions match: ", target.size()));
        } else {
            showMessage(MsgLevel::MSG_ERR, stringify(
                    "number of dimensions (", ndims,
                    ") does not match length of target (", target.size(), ")."));
        }
    };

    std::vector<double> RegularGridInterpolator::interpolation_wrapper() {
        std::size_t ndims = get_ndims();
        std::vector<Method> methods = the_locator.get_methods();
        std::vector<Method> base_methods = grid_data.get_interp_methods();
        Eigen::ArrayXd result;

        if (std::equal(methods.begin(), methods.end(), base_methods.begin())) {
            hypercube.collect_things(the_locator);
            result = hypercube.all_the_calculations(grid_data);
        } else {
            Hypercube new_hypercube(ndims, methods);
            new_hypercube.collect_things(the_locator);
            result = new_hypercube.all_the_calculations(grid_data);
        }
        showMessage(MsgLevel::MSG_DEBUG, stringify("results\n", result));
        return eigen_to_vector(result);
    }

// free functions
    std::size_t pow(const std::size_t &base, const std::size_t &power) {
        // raise base to a power (both must be size_t)
        if (power == 0) { return 1; }
        else {
            std::size_t result = base;
            for (std::size_t i = 1; i < power; i++) {
                result = result * base;
            }
            return result;
        }
    }


}
