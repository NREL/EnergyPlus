//
// Created by jackcook on 5/10/21.
//

#include <cpgfunction/statistics.h>

namespace gt {
    namespace statistics {

        double root_mean_square_error(std::vector<double> actual, std::vector<double> predicted) {
            // Return the root mean square error between two g-function curves

            double square_error = 0;

            for (int i = 0; i < actual.size(); i++) {
                square_error += std::pow(((actual[i] - predicted[i]) / actual[i]), 2);
            }  // next i

            double rmse = std::sqrt(square_error / double(actual.size()));

            return rmse;
        }

    }  // namespace statistics
}  // namespace gt