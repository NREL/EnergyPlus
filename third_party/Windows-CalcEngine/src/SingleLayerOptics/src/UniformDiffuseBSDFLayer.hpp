#ifndef UniformDiffuseShade_H
#define UniformDiffuseShade_H

#include <memory>

#include "BSDFLayer.hpp"

namespace SingleLayerOptics
{
    class CUniformDiffuseCell;

    // All outgoing directions are uniformly distributed in all directions
    class CUniformDiffuseBSDFLayer : public CBSDFLayer
    {
    public:
        CUniformDiffuseBSDFLayer(const std::shared_ptr<CUniformDiffuseCell> & t_Cell,
                                 const CBSDFHemisphere & t_Hemisphere);

    protected:
        std::shared_ptr<CUniformDiffuseCell> cellAsUniformDiffuse() const;
        void calcDiffuseDistribution(FenestrationCommon::Side aSide,
                                     const CBeamDirection & t_Direction,
                                     size_t t_DirectionIndex) override;
        void calcDiffuseDistribution_wv(FenestrationCommon::Side aSide,
                                        const CBeamDirection & t_Direction,
                                        size_t t_DirectionIndex) override;
    };

}   // namespace SingleLayerOptics

#endif
