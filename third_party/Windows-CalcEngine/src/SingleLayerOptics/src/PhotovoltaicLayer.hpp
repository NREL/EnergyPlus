#pragma once

#include <memory>

#include "SpecularLayer.hpp"

namespace SingleLayerOptics
{
    class CMaterialPhotovoltaic;

    ////////////////////////////////////////////////////////////////////////////////////////////////
    // PhotovoltaicLayer
    ////////////////////////////////////////////////////////////////////////////////////////////////

    class PhotovoltaicLayer : public SpecularLayer
    {
    public:
        static std::shared_ptr<PhotovoltaicLayer>
          createLayer(const std::shared_ptr<CMaterialPhotovoltaic> & material);

        FenestrationCommon::CSeries PCE(FenestrationCommon::Side t_Side) const;
        FenestrationCommon::CSeries W(FenestrationCommon::Side t_Side) const;

        explicit PhotovoltaicLayer(const CSpecularCell & cell,
                                   const std::shared_ptr<CMaterialPhotovoltaic> & material);

    private:
        std::shared_ptr<CMaterialPhotovoltaic> m_PVMaterial;
    };

}   // namespace SingleLayerOptics