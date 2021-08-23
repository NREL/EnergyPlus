//
// Created by jackcook on 7/11/20.
//

#include <iostream>
#include <vector>
#include <cmath>
#include <cpgfunction/boreholes.h>
#include <cpgfunction/segments.h>

using namespace std;
using namespace gt;

#ifndef CPPGFUNCTION_HEAT_TRANSFER_H
#define CPPGFUNCTION_HEAT_TRANSFER_H

namespace gt::heat_transfer {

    double finite_line_source(double time_, double alpha,
                              boreholes::Borehole& b1, boreholes::Borehole& b2,
                              bool reaSource=true, bool imgSource=true);
    void thermal_response_factors(gt::segments::SegmentResponse &SegRes,
                                  vector<double>& time, double alpha,
                                  bool use_similaries, bool disp=false);

} // namespace gt::heat_transfer

#endif //CPPGFUNCTION_HEAT_TRANSFER_H
