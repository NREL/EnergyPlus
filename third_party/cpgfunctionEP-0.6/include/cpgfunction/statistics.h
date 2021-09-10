//
// Created by jackcook on 5/10/21.
//

#ifndef CPGFUNCTION_STATISTICS_H
#define CPGFUNCTION_STATISTICS_H

#include <cmath>
#include <iostream>
#include <vector>

namespace gt {
    namespace statistics {

        double root_mean_square_error(std::vector<double> actual, std::vector<double> predicted);

    }  // namespace statistics
}  // namespace gt

#endif //CPGFUNCTION_STATISTICS_H
