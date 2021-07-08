//
// Created by jackcook on 2/27/21.
//

#include <iostream>
#include <vector>
#include <cpgfunction/interpolation.h>

int main() {
    // 1D interpolation test
    std::vector<double>  x {0, 200, 400, 600};
    std::vector<double>  y {373.0, 156.1, 113.6, 93.1};
    std::vector<double>  xp {90, 210, 310};
    std::vector<double>  yp (3);
    jcc::interpolation::interp1d(xp, yp, x, y);

    std::cout << "The y points for linear interpolation" << std::endl;
    for (const auto& i: yp) {
        std:: cout << i << std::endl;
    }

    return 0;
}