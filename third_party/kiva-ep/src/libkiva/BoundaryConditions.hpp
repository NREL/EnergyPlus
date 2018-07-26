/* Copyright (c) 2012-2017 Big Ladder Software LLC. All rights reserved.
* See the LICENSE file for additional terms and conditions. */

#ifndef BoundaryConditions_HPP
#define BoundaryConditions_HPP

namespace Kiva {

class BoundaryConditions {
public:
  double indoorTemp;
  double indoorRadiantTemp;
  double outdoorTemp;
  double localWindSpeed;
  double solarAzimuth;
  double solarAltitude;
  double directNormalFlux;
  double diffuseHorizontalFlux;
  double skyEmissivity;
  double slabAbsRadiation;
  double wallAbsRadiation;

  BoundaryConditions() :
    indoorTemp(293.15),
    indoorRadiantTemp(293.15),
    outdoorTemp(273.15),
    localWindSpeed(0.0),
    solarAzimuth(3.14),
    solarAltitude(0.0),
    directNormalFlux(0.0),
    diffuseHorizontalFlux(0.0),
    skyEmissivity(0.0),
    slabAbsRadiation(0.0),
    wallAbsRadiation(0.0)
  {}

};

}
#endif // BoundaryConditions_HPP
