//
// Created by jackcook on 7/11/20.
//

#ifndef CPPGFUNCTION_HEAT_TRANSFER_H
#define CPPGFUNCTION_HEAT_TRANSFER_H

#include <cmath>
#include <cpgfunction/boreholes.h>
#include <cpgfunction/segments.h>
#include <iostream>
#include <vector>

namespace gt::heat_transfer {

    double finite_line_source(double time_, double alpha,
                              boreholes::Borehole& b1, boreholes::Borehole& b2,
                              bool reaSource=true, bool imgSource=true);
    void thermal_response_factors(gt::segments::SegmentResponse &SegRes,
                                  std::vector<double>& time, double alpha,
                                  bool use_similarities, bool disp=false, int numThreads=1);

} // namespace gt::heat_transfer

#endif //CPPGFUNCTION_HEAT_TRANSFER_H
