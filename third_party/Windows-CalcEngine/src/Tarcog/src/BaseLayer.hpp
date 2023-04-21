#ifndef BASELAYER_H
#define BASELAYER_H

#include "LayerInterfaces.hpp"

#include <memory>

namespace FenestrationCommon
{
    enum class Side;
}

namespace Tarcog
{
    namespace ISO15099
    {
        // Base description for any tarcog layer. This includes interior and exterior environments
        // as well. It must contain base definition of 2D geometry (Width and Height) and definition
        // of heat flow that is divided in three categories (convection, conduction and radiation).
        // Every layer can contain only Conduction + Radiation or Convection + Radiation.
        class CBaseLayer : public CLayerGeometry,
                           public CLayerHeatFlow,
                           public std::enable_shared_from_this<CBaseLayer>
        {
        public:
            CBaseLayer();

            std::shared_ptr<CBaseLayer> getPreviousLayer() const;
            std::shared_ptr<CBaseLayer> getNextLayer() const;

            virtual void connectToBackSide(std::shared_ptr<CBaseLayer> const & t_Layer);

            void tearDownConnections();

            virtual double getThickness() const;

            // This is to determine if layer is porous and leaking air from gap to the surrounding
            // environment. Layer are non-porous by default.
            virtual bool isPermeable() const;

            virtual std::shared_ptr<CBaseLayer> clone() const = 0;

        protected:
            void calculateRadiationFlow() override;
            void calculateConvectionOrConductionFlow() override = 0;

            std::shared_ptr<CBaseLayer> m_PreviousLayer;
            std::shared_ptr<CBaseLayer> m_NextLayer;
        };
    }   // namespace ISO15099

}   // namespace Tarcog

#endif
