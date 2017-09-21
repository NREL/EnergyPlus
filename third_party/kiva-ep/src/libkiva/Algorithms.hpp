/* Copyright (c) 2012-2017 Big Ladder Software LLC. All rights reserved.
* See the LICENSE file for additional terms and conditions. */

#ifndef ConvectionAlgorithms_HPP
#define ConvectionAlgorithms_HPP

#include <cmath>
//#include "PixelCounter.hpp"

namespace Kiva {
// TODO: use defaulting inputs
double getDOE2ConvectionCoeff(double tilt,
                          double azimuth,
                          double windDirection,
                          double Tsurf,
                          double Tamb,
                          double Vair,
                          double roughness);

bool isWindward(double tilt, double azimuth, double windDirection);

double getExteriorIRCoeff(double eSurf,
                      double Tsurf,
                      double Tamb,
                      double eSky,
                      double tilt);

/*
double getExteriorSolarGain(double aSurf,
                        double Idn,
              double Idiff,
              double Igh,
              double orientation,
              double azimuth,
              double altitude


                );
*/

double getEffectiveExteriorViewFactor(double eSky,
                                  double tilt);

double getSimpleInteriorIRCoeff(double eSurf,
                            double Tsurf,
                            double Tamb);

}

#endif
