/* Copyright (c) 2018 Big Ladder Software LLC. All rights reserved.
* See the LICENSE file for additional terms and conditions. */

#ifndef GRIDPOINT_H_
#define GRIDPOINT_H_

// Standard
#include<vector>

// btwxt
#include "griddeddata.h"

namespace Btwxt {

    enum class Bounds {
        OUTLAW, OUTBOUNDS, INBOUNDS
    };

    class GridPoint {
    public:
        // target is an array of doubles specifying the point we are interpolating to.
        GridPoint();

        explicit GridPoint(const std::vector<double> &target_vector);

        std::vector<double> target;
    };


    class PointLocator {
    public:
        PointLocator();

        PointLocator(GridPoint &, GriddedData &);

        std::vector<std::size_t> get_floor();

        std::vector<double> get_weights();

        std::vector<Bounds> get_is_inbounds();

        std::vector<Method> get_methods();

        std::vector<std::vector<double> > get_interp_coeffs();

        std::vector<std::vector<double> > get_cubic_slope_coeffs();

    private:
        std::size_t ndims;
        std::vector<std::size_t> point_floor;  // index of grid point <= target
        std::vector<double> weights;
        std::vector<Bounds> is_inbounds;  // for deciding interpolation vs. extrapolation;
        std::vector<Method> methods;
        std::vector<std::vector<double> > interp_coeffs;
        std::vector<std::vector<double> > cubic_slope_coeffs;

        void find_floor(GridPoint &, GriddedData &);

        void calculate_weights(GridPoint &, GriddedData &);

        void consolidate_methods(const std::vector<Method> &interp_methods,
                                 const std::vector<Method> &extrap_methods);

        void calculate_interp_coeffs();
    };


    // free functions
    void locate_in_dim(const double &target, Bounds &dim_in, std::size_t &dim_floor,
                       std::vector<double> grid_vector, std::pair<double, double> &extrap_limits);

    std::size_t index_below_in_vector(const double &target, std::vector<double> &my_vec);

    double compute_fraction(double x, double edge[2]);
}

#endif // GRIDPOINT_H_
