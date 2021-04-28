#ifndef BSDFLAYERMAKER_H
#define BSDFLAYERMAKER_H

#include <memory>

namespace SingleLayerOptics
{
    enum class DistributionMethod
    {
        UniformDiffuse,
        DirectionalDiffuse
    };

    class ICellDescription;
    class CMaterial;
    class CBSDFHemisphere;
    class CBSDFLayer;
    class CBaseCell;

    // Class to simplify interface for BSDF layer creation
    class CBSDFLayerMaker
    {
    public:
        static std::shared_ptr<CBSDFLayer>
          getSpecularLayer(const std::shared_ptr<CMaterial> & t_Material,
                           const CBSDFHemisphere & t_BSDF);

        static std::shared_ptr<CBSDFLayer>
          getCircularPerforatedLayer(const std::shared_ptr<CMaterial> & t_Material,
                                     const CBSDFHemisphere & t_BSDF,
                                     double x,
                                     double y,
                                     double thickness,
                                     double radius);

        static std::shared_ptr<CBSDFLayer>
          getRectangularPerforatedLayer(const std::shared_ptr<CMaterial> & t_Material,
                                        const CBSDFHemisphere & t_BSDF,
                                        double x,
                                        double y,
                                        double thickness,
                                        double xHole,
                                        double yHole);

        static std::shared_ptr<CBSDFLayer>
          getVenetianLayer(const std::shared_ptr<CMaterial> & t_Material,
                           const CBSDFHemisphere & t_BSDF,
                           double slatWidth,
                           double slatSpacing,
                           double slatTiltAngle,
                           double curvatureRadius,
                           size_t numOfSlatSegments,
                           DistributionMethod method = DistributionMethod::DirectionalDiffuse);

        static std::shared_ptr<CBSDFLayer>
          getPerfectlyDiffuseLayer(const std::shared_ptr<CMaterial> & t_Material,
                                   const CBSDFHemisphere & t_BSDF);

        static std::shared_ptr<CBSDFLayer>
          getDirectionalDiffuseLayer(const std::shared_ptr<CMaterial> & t_Material,
                                     const CBSDFHemisphere & t_BSDF);

        static std::shared_ptr<CBSDFLayer>
          getPreLoadedBSDFLayer(const std::shared_ptr<CMaterial> & t_Material,
                                const CBSDFHemisphere & t_BSDF);

        static std::shared_ptr<CBSDFLayer>
          getWovenLayer(const std::shared_ptr<CMaterial> & t_Material,
                        const CBSDFHemisphere & t_BSDF,
                        double diameter,
                        double spacing);

        CBSDFLayerMaker(const std::shared_ptr<CMaterial> & t_Material,
                        const CBSDFHemisphere & t_BSDF,
                        std::shared_ptr<ICellDescription> t_Description = nullptr,
                        const DistributionMethod t_Method = DistributionMethod::UniformDiffuse);

        [[nodiscard]] std::shared_ptr<CBSDFLayer> getLayer() const;
        [[nodiscard]] std::shared_ptr<CBaseCell> getCell() const;


    private:
        std::shared_ptr<CBSDFLayer> m_Layer;
        std::shared_ptr<CBaseCell> m_Cell;
    };

}   // namespace SingleLayerOptics

#endif
