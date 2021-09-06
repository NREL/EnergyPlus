//
// Created by jackcook on 7/15/20.
//

#ifndef CPPGFUNCTION_INTERPOLATION_H
#define CPPGFUNCTION_INTERPOLATION_H

#include <cpgfunction/segments.h>
#include <iostream>
#include <vector>

namespace jcc::interpolation {

    double linterp(double xp, double x0, double y0, double x1, double y1);
    void interp1d(std::vector<double>& xp, std::vector<double>& yp, std::vector<double>& x,
                  std::vector<double>& y);
    double interp1d(double &xp, std::vector<double>& x, std::vector<double>& y);
    void interp1d(double &xp, double &yp, std::vector<double> &time,
                  gt::segments::SegmentResponse &SegRes, int &i, int &j, int &k);

} // jcc::interpolation

#endif //CPPGFUNCTION_INTERPOLATION_H
