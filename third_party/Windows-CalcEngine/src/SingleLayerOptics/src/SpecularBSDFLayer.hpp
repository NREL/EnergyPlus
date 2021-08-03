#ifndef SPECULARBSDFLAYER_H
#define SPECULARBSDFLAYER_H

#include <memory>
#include <vector>

#include "BSDFLayer.hpp"

namespace SingleLayerOptics
{
    class CSpecularCell;

    // BSDF creation for specular layers.
    class CSpecularBSDFLayer : public CBSDFLayer
    {
    public:
        CSpecularBSDFLayer(const std::shared_ptr<CSpecularCell> & t_Cell,
                           const CBSDFHemisphere & t_Hemisphere);

    protected:
        std::shared_ptr<CSpecularCell> cellAsSpecular() const;
        void calcDiffuseDistribution(FenestrationCommon::Side aSide,
                                     const CBeamDirection & t_Direction,
                                     size_t t_DirectionIndex) override;
        void calcDiffuseDistribution_wv(FenestrationCommon::Side aSide,
                                        const CBeamDirection & t_Direction,
                                        size_t t_DirectionIndex) override;
    };

}   // namespace SingleLayerOptics

#endif
