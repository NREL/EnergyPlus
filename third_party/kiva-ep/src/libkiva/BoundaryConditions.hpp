/* Copyright (c) 2012-2018 Big Ladder Software LLC. All rights reserved.
 * See the LICENSE file for additional terms and conditions. */

#ifndef BoundaryConditions_HPP
#define BoundaryConditions_HPP

namespace Kiva {

class BoundaryConditions {
public:
  double indoorTemp;
  double slabRadiantTemp;
  double wallRadiantTemp;
  double outdoorTemp;
  double localWindSpeed;
  double windDirection;
  double solarAzimuth;
  double solarAltitude;
  double directNormalFlux;
  double diffuseHorizontalFlux;
  double skyEmissivity;
  double slabAbsRadiation;
  double wallAbsRadiation;
  double deepGroundTemperature;

  BoundaryConditions()
      : indoorTemp(293.15), slabRadiantTemp(293.15), wallRadiantTemp(293.15), outdoorTemp(273.15),
        localWindSpeed(0.0), windDirection(0.0), solarAzimuth(3.14), solarAltitude(0.0),
        directNormalFlux(0.0), diffuseHorizontalFlux(0.0), skyEmissivity(0.0),
        slabAbsRadiation(0.0), wallAbsRadiation(0.0), deepGroundTemperature(283.15) {}
};

} // namespace Kiva
#endif // BoundaryConditions_HPP
