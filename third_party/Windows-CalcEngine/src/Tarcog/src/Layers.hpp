#pragma once

#include <memory>
#include <WCEGases.hpp>

#include "EffectiveOpenness.hpp"
#include "TarcogConstants.hpp"

namespace Tarcog
{
    namespace ISO15099
    {
        class CIGUSolidLayer;

        class CIGUGapLayer;

        class Layers
        {
        public:
            static std::shared_ptr<CIGUSolidLayer> solid(double thickness,
                                                         double conductivity,
                                                         double frontEmissivity = 0.84,
                                                         double frontIRTransmittance = 0.0,
                                                         double backEmissivity = 0.84,
                                                         double backIRTransmittance = 0.0);

            static std::shared_ptr<CIGUSolidLayer>
              updateMaterialData(const std::shared_ptr<CIGUSolidLayer> & layer,
                                 double density = MaterialConstants::GLASSDENSITY,
                                 double youngsModulus = DeflectionConstants::YOUNGSMODULUS);

            static std::shared_ptr<CIGUSolidLayer>
              shading(double thickness,
                      double conductivity,
                      EffectiveLayers::EffectiveOpenness effectiveOpenness =
                        EffectiveLayers::EffectiveOpenness(0, 0, 0, 0, 0, 0),
                      double frontEmissivity = 0.84,
                      double frontTransmittance = 0.0,
                      double backEmissivity = 0.84,
                      double backTransmittance = 0.0);

            static std::shared_ptr<CIGUGapLayer> gap(double thickness, double pressure = 101325);
            static std::shared_ptr<CIGUGapLayer>
              gap(double thickness, const Gases::CGas & gas, double pressure = 101325);

            static std::shared_ptr<CIGUGapLayer>
              addCircularPillar(const std::shared_ptr<CIGUGapLayer> & gap,
                                double conductivity,
                                double spacing,
                                double radius);
        };

    }   // namespace ISO15099

}   // namespace Tarcog
