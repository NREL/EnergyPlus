#include "Layers.hpp"
#include "IGUGapLayer.hpp"
#include "IGUSolidLayer.hpp"
#include "IGUSolidDeflection.hpp"
#include "BaseShade.hpp"
#include "Surface.hpp"
#include "SupportPillar.hpp"
#include "EffectiveOpenness.hpp"

namespace Tarcog
{
    namespace ISO15099
    {
        std::shared_ptr<CIGUSolidLayer> Layers::solid(const double thickness,
                                                      const double conductivity,
                                                      const double frontEmissivity,
                                                      const double frontIRTransmittance,
                                                      const double backEmissivity,
                                                      const double backIRTransmittance)
        {
            return std::make_shared<CIGUSolidLayer>(
              thickness,
              conductivity,
              std::make_shared<CSurface>(frontEmissivity, frontIRTransmittance),
              std::make_shared<CSurface>(backEmissivity, backIRTransmittance));
        }

        std::shared_ptr<CIGUGapLayer> Layers::gap(const double thickness, const double pressure)
        {
            return std::make_shared<CIGUGapLayer>(thickness, pressure);
        }

        std::shared_ptr<CIGUGapLayer>
          Layers::gap(double thickness, const Gases::CGas & gas, double pressure)
        {
            return std::make_shared<CIGUGapLayer>(thickness, pressure, gas);
        }

        std::shared_ptr<CIGUSolidLayer> Layers::updateMaterialData(
          const std::shared_ptr<CIGUSolidLayer> & layer, double density, double youngsModulus)
        {
            // Deflection cannot be applied to shading layers
            if(std::dynamic_pointer_cast<CIGUShadeLayer>(layer) == nullptr)
            {
                static const double poissonRatio{0.22};
                return std::make_shared<CIGUSolidLayerDeflection>(
                  *layer, youngsModulus, poissonRatio, density);
            }
            else
            {
                return layer;
            }
        }

        std::shared_ptr<CIGUSolidLayer>
          Layers::shading(double thickness,
                          double conductivity,
                          EffectiveLayers::EffectiveOpenness effectiveOpenness,
                          double frontEmissivity,
                          double frontTransmittance,
                          double backEmissivity,
                          double backTransmittance)
        {
            if(effectiveOpenness.isClosed())
            {
                return solid(thickness,
                             conductivity,
                             frontEmissivity,
                             frontTransmittance,
                             backEmissivity,
                             backTransmittance);
            }

            return std::make_shared<CIGUShadeLayer>(
              thickness,
              conductivity,
              std::make_shared<CShadeOpenings>(effectiveOpenness.Atop,
                                               effectiveOpenness.Abot,
                                               effectiveOpenness.Al,
                                               effectiveOpenness.Ar,
                                               effectiveOpenness.Ah,
                                               effectiveOpenness.FrontPorosity),
              std::make_shared<CSurface>(frontEmissivity, frontTransmittance),
              std::make_shared<CSurface>(backEmissivity, backTransmittance));
        }

        std::shared_ptr<CIGUGapLayer>
          Layers::addCircularPillar(const std::shared_ptr<CIGUGapLayer> & gap,
                                    double conductivity,
                                    double spacing,
                                    double radius)
        {
            return std::make_shared<CCircularPillar>(*gap, conductivity, spacing, radius);
        }

    }   // namespace ISO15099

}   // namespace Tarcog
