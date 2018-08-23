/* Copyright (c) 2018 Big Ladder Software LLC. All rights reserved.
* See the LICENSE file for additional terms and conditions. */


// Standard
#include <iostream>
#include <numeric>

//btwxt
#include "error.h"

namespace Btwxt {


    GridPoint::GridPoint() {};

    GridPoint::GridPoint(const std::vector<double> &target_vector) :
            target(target_vector) {
        showMessage(MsgLevel::MSG_DEBUG, "GridPoint object constructed from vector!");
    };


    PointLocator::PointLocator() {};

    PointLocator::PointLocator(
            GridPoint &current_grid_point, GriddedData &the_blob
    ) :
            ndims(the_blob.get_ndims()),
            point_floor(ndims, 0),
            weights(ndims, 0),
            is_inbounds(ndims),
            interp_coeffs(ndims, std::vector<double>(2, 0.0)),
            cubic_slope_coeffs(ndims, std::vector<double>(2, 0.0)) {
        find_floor(current_grid_point, the_blob);
        calculate_weights(current_grid_point, the_blob);
        consolidate_methods(the_blob.get_interp_methods(),
                            the_blob.get_extrap_methods());
        calculate_interp_coeffs();
    };

    std::vector<std::size_t> PointLocator::get_floor() { return point_floor; }

    std::vector<double> PointLocator::get_weights() { return weights; }

    std::vector<Bounds> PointLocator::get_is_inbounds() { return is_inbounds; }

    std::vector<Method> PointLocator::get_methods() { return methods; }

    std::vector<std::vector<double> > PointLocator::get_interp_coeffs() { return interp_coeffs; }

    std::vector<std::vector<double> > PointLocator::get_cubic_slope_coeffs() { return cubic_slope_coeffs; }

    void PointLocator::find_floor(
            GridPoint &current_grid_point, GriddedData &the_blob) {
        for (std::size_t dim = 0; dim < ndims; dim += 1) {
            double value {current_grid_point.target[dim]};
            std::pair<double, double> extrap_limits = the_blob.get_extrap_limits(dim);
            std::vector<double> grid_vector = the_blob.get_grid_vector(dim);
            locate_in_dim(value, is_inbounds[dim], point_floor[dim], grid_vector, extrap_limits);
        }
    }

    void PointLocator::calculate_weights(
            GridPoint &current_grid_point, GriddedData &the_blob) {
        for (std::size_t dim = 0; dim < ndims; dim += 1) {
            std::vector<double> grid_vector = the_blob.get_grid_vector(dim);
            double edge[] = {grid_vector[point_floor[dim]], grid_vector[point_floor[dim] + 1]};
            weights[dim] = compute_fraction(current_grid_point.target[dim], edge);
        }
    }

    void PointLocator::consolidate_methods(
            const std::vector<Method> &interp_methods,
            const std::vector<Method> &extrap_methods)
    // If out of bounds, extrapolate according to prescription
    // If outside of extrapolation limits, send a warning and perform constant extrapolation.
    {
        methods = interp_methods;
        for (std::size_t dim = 0; dim < ndims; dim++) {
            if (is_inbounds[dim] == Bounds::OUTBOUNDS) {
                methods[dim] = extrap_methods[dim];
            } else if (is_inbounds[dim] == Bounds::OUTLAW) {
                showMessage(MsgLevel::MSG_WARN, stringify("The target is outside the extrapolation limits in dimension ", dim,
                                                ". Will perform constant extrapolation."));
                methods[dim] = Method::CONSTANT;
            }
        }
    }

    void PointLocator::calculate_interp_coeffs() {
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
            } else {  // LINEAR
                interp_coeffs[dim][0] = 1 - mu;
                interp_coeffs[dim][1] = mu;
            }
        }
    }



    // free functions
    void locate_in_dim(const double& target, Bounds& dim_in, std::size_t& dim_floor,
                       std::vector<double> grid_vector, std::pair<double, double> &extrap_limits)
    {
        std::size_t l = grid_vector.size();
        if (target < extrap_limits.first) {
            dim_in = Bounds::OUTLAW;
            dim_floor = 0;
        } else if (target > extrap_limits.second) {
            dim_in = Bounds::OUTLAW;
            dim_floor = l-2;  // l-2 because that's the left side of the (l-2, l-1) edge.
        } else if (target < grid_vector[0]) {
            dim_in = Bounds::OUTBOUNDS;
            dim_floor = 0;
        } else if (target > grid_vector.back()) {
            dim_in = Bounds::OUTBOUNDS;
            dim_floor = l-2;  // l-2 because that's the left side of the (l-2, l-1) edge.
        } else {
            dim_in = Bounds::INBOUNDS;
            dim_floor = index_below_in_vector(target, grid_vector);
        }
    }

    std::size_t index_below_in_vector(const double& target, std::vector<double> &my_vec)
    // returns the index of the largest value <= the target
    {
        std::vector<double>::iterator upper;
        upper = std::upper_bound(my_vec.begin(), my_vec.end(), target);
        if ((upper - my_vec.begin()) == 0) {
            showMessage(MsgLevel::MSG_ERR, "index_below_in_vector should only be called once you are sure you are inbounds");
            return 0;
        }
        return upper - my_vec.begin() - 1;
    }

    double compute_fraction(double x, double edge[2]) {
        // how far along an edge is the target?
        return (x - edge[0]) / (edge[1] - edge[0]);
    }

}
