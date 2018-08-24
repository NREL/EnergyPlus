/* Copyright (c) 2018 Big Ladder Software LLC. All rights reserved.
* See the LICENSE file for additional terms and conditions. */


// Standard
#include <iostream>
#include <numeric>

// btwxt
#include "hypercube.h"
#include "error.h"

namespace Btwxt {


    Hypercube::Hypercube() = default;

    Hypercube::Hypercube(const std::size_t &ndims,
                         const std::vector<Method> &methods) :
            ndims(ndims),
            vertices(make_hypercube(ndims, methods)),
            methods(methods)
    {
        collection.resize(ndims,std::vector<double>(2));
        sivor = { {-1,0},{-1,1},{1,0},{1,1} };
    }

    void Hypercube::collect_things(PointLocator &the_locator) {
        point_floor = the_locator.get_floor();
        interp_coeffs = the_locator.get_interp_coeffs();
        cubic_slope_coeffs = the_locator.get_cubic_slope_coeffs();
    }

    Eigen::ArrayXd Hypercube::all_the_calculations(GriddedData &the_blob) {

        Eigen::ArrayXd result = Eigen::ArrayXd::Zero(the_blob.get_num_tables());
        Eigen::ArrayXd values = Eigen::ArrayXd::Zero(the_blob.get_num_tables());
        double weight;

        std::vector< std::vector<double> > spacing_mults = get_spacing_mults(the_blob);
        for (const auto& v: vertices) {
            weight = get_vertex_weight(v, spacing_mults);
            values = the_blob.get_column_near_safe(point_floor, v);
//            showMessage(MSG_INFO, stringify("vertex total = \n", values*weight));
            result += values * weight;
        }
//        showMessage(MSG_INFO, stringify("complete answer = \n", result));
        return result;
    }

    std::vector< std::vector<double> > Hypercube::get_spacing_mults(GriddedData &the_blob) {
        std::vector< std::vector<double> > spacing_mults(ndims, std::vector<double>(2, 0));
        for (std::size_t dim=0; dim<ndims; dim++) {
            if (methods[dim] == Method::CUBIC) {
                spacing_mults[dim][0] = the_blob.get_axis_spacing_mult(dim, 0, point_floor[dim]);
                spacing_mults[dim][1] = the_blob.get_axis_spacing_mult(dim, 1, point_floor[dim]);
            }
        }
        return spacing_mults;
    }

    double Hypercube::get_vertex_weight(const std::vector<int> &v,
                const std::vector< std::vector<double> >& spacing_mults) {
        int sign, flavor;
        for (std::size_t dim = 0; dim < ndims; dim++) {
            if (methods[dim] == Method::CUBIC) {
                std::tie(sign, flavor) = sivor[v[dim] + 1];
                if (v[dim] == 0 || v[dim] == 1) { // closest neighbors have a normal interpolation term
                    collection[dim][0] = interp_coeffs[dim][v[dim]];
                } else {
                    collection[dim][0] = 0.0;
                }
                collection[dim][1] = cubic_slope_coeffs[dim][flavor]
                                          * spacing_mults[dim][flavor] * sign;
            } else {  // LINEAR or CONSTANT
                collection[dim][0] = interp_coeffs[dim][v[dim]];
                collection[dim][1] = 0.0;
            }
        }
        return sum_weighting_terms(collection);
    }


// free functions
    std::vector<std::vector<int> > make_hypercube(
            const std::size_t &ndims) {
        std::vector<std::vector<int> > options(ndims, {0, 1});
        return cart_product(options);
    }

    std::vector<std::vector<int> > make_hypercube(
            const std::size_t &ndims, const std::vector<Method> &methods) {
        std::vector<std::vector<int> > options(ndims, {0, 1});

        for (std::size_t dim = 0; dim < ndims; dim++) {
            if (methods[dim] == Method::CUBIC) {
                options[dim] = {-1, 0, 1, 2};
            }
        }
        return cart_product(options);
    }

    template <typename T>
    std::vector<std::vector<T>> cart_product(const std::vector<std::vector<T> > &v) {
        std::vector<std::vector<T> > combinations = {{}};
        for (const auto &list : v) {
            std::vector<std::vector<T> > r;
            for (const auto &x : combinations) {
                for (const auto item : list) {
                    r.push_back(x);
                    r.back().push_back(item);
                }
            }
            combinations = std::move(r);
        }
        return combinations;
    }

    double sum_weighting_terms(const std::vector< std::vector<double> >& v ) {
        std::size_t nV = v.size();
        std::size_t N = 1;
        for (std::size_t dim=0; dim < nV; ++dim) { N *= 2; } // Two options per dimension (normal or slope terms)


        // Add all combinations of coefficient term products
        double weight_sum = 0.0;
        for(std::size_t n=0 ; n < N ; ++n ) {
            double product = 1.0;
            for(std::size_t dim=0; dim < nV; ++dim) {
               product *= v[dim][bool(n & 1<<dim)];
            }
            weight_sum += product;
        }
        return weight_sum;
    }

}
