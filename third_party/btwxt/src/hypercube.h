/* Copyright (c) 2018 Big Ladder Software LLC. All rights reserved.
* See the LICENSE file for additional terms and conditions. */

#ifndef HYPER_H_
#define HYPER_H_

#include<vector>
#include "Eigen/Dense"

// btwxt
#include "griddeddata.h"
#include "gridpoint.h"

namespace Btwxt {
    

    class Hypercube {
    public:
        Hypercube();

        Hypercube(const std::size_t &ndims, const std::vector<Method> &methods);

        void collect_things(PointLocator &);

        std::size_t ndims;
        std::vector<std::vector<int> > vertices;

        Eigen::ArrayXd all_the_calculations(GriddedData &the_blob);

        std::vector< std::vector<double> > get_spacing_mults(GriddedData &the_blob);

        double get_vertex_weight(const std::vector<int> &v,
                const std::vector< std::vector<double> >& spacing_mults);

        std::vector<std::size_t> point_floor;
        std::vector<Method> methods;
        std::vector<std::vector<double> > cubic_slope_coeffs;
        std::vector<std::vector<double> > interp_coeffs;
        std::vector<std::vector<double> > collection;
        std::vector< std::pair<int, int> > sivor;  // {sign, flavor}, flavor corresponds to the floor (0) or ceiling (1) bounding of the point
    };


// free functions
    std::vector<std::vector<int> > make_hypercube(const std::size_t &ndims);
    std::vector<std::vector<int> > make_hypercube(
            const std::size_t &ndims, const std::vector<Method> &methods);

    template <typename T>
    std::vector<std::vector<T> > cart_product(const std::vector<std::vector<T> > &v);

    double sum_weighting_terms(const std::vector<std::vector<double> > &v);

}
#endif // HYPER_H_
