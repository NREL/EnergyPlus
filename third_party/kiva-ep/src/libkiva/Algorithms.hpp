/* Copyright (c) 2012-2018 Big Ladder Software LLC. All rights reserved.
 * See the LICENSE file for additional terms and conditions. */

#ifndef ConvectionAlgorithms_HPP
#define ConvectionAlgorithms_HPP

#include <cmath>
#include <map>

//#include "PixelCounter.hpp"

#define MEMOIZE_DECL(NAME)                                                                         \
  namespace Memo {                                                                                 \
  static std::map<double, double> NAME##_map;                                                      \
  double NAME(double x);                                                                           \
  }

namespace Kiva {

MEMOIZE_DECL(cos)
MEMOIZE_DECL(pow025)
MEMOIZE_DECL(pow089)
MEMOIZE_DECL(pow0617)

double cbrt_a(double x);

// TODO: use defaulting inputs
double getDOE2ConvectionCoeff(double Tsurf, double Tamb, double hfGlass, double roughness,
                              double cosTilt);

bool isWindward(double cosTilt, double azimuth, double windDirection);

double getExteriorIRCoeff(double eSurf, double Tsurf, double Tamb, double Fqtr);

double getEffectiveExteriorViewFactor(double eSky, double tilt);

double getSimpleInteriorIRCoeff(double eSurf, double Tsurf, double Trad);

} // namespace Kiva

#endif
