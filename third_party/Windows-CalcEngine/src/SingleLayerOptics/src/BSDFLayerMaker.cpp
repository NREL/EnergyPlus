#include <stdexcept>

#include "BSDFLayerMaker.hpp"
#include "UniformDiffuseCell.hpp"
#include "DirectionalDiffuseCell.hpp"
#include "UniformDiffuseBSDFLayer.hpp"
#include "DirectionalDiffuseBSDFLayer.hpp"
#include "CellDescription.hpp"
#include "SpecularCellDescription.hpp"
#include "SpecularCell.hpp"
#include "SpecularBSDFLayer.hpp"
#include "VenetianCellDescription.hpp"
#include "VenetianCell.hpp"
#include "PerforatedCellDescription.hpp"
#include "PerforatedCell.hpp"
#include "WovenCellDescription.hpp"
#include "WovenCell.hpp"
#include "FlatCellDescription.hpp"

namespace SingleLayerOptics
{
    std::shared_ptr<CBSDFLayer>
      CBSDFLayerMaker::getSpecularLayer(const std::shared_ptr<CMaterial> & t_Material,
                                        const CBSDFHemisphere & t_BSDF)
    {
        auto aDescription = std::make_shared<CSpecularCellDescription>();
        auto aCell = std::make_shared<CSpecularCell>(t_Material, aDescription);
        return std::make_shared<CSpecularBSDFLayer>(aCell, t_BSDF);
    }

    std::shared_ptr<CBSDFLayer>
      CBSDFLayerMaker::getCircularPerforatedLayer(const std::shared_ptr<CMaterial> & t_Material,
                                                  const CBSDFHemisphere & t_BSDF,
                                                  double x,
                                                  double y,
                                                  double thickness,
                                                  double radius)
    {
        std::shared_ptr<ICellDescription> aCellDescription =
          std::make_shared<CCircularCellDescription>(x, y, thickness, radius);
        std::shared_ptr<CUniformDiffuseCell> aCell =
          std::make_shared<CPerforatedCell>(t_Material, aCellDescription);
        return std::make_shared<CUniformDiffuseBSDFLayer>(aCell, t_BSDF);
    }

    std::shared_ptr<CBSDFLayer>
      CBSDFLayerMaker::getRectangularPerforatedLayer(const std::shared_ptr<CMaterial> & t_Material,
                                                     const CBSDFHemisphere & t_BSDF,
                                                     double x,
                                                     double y,
                                                     double thickness,
                                                     double xHole,
                                                     double yHole)
    {
        std::shared_ptr<ICellDescription> aCellDescription =
          std::make_shared<CRectangularCellDescription>(x, y, thickness, xHole, yHole);
        std::shared_ptr<CUniformDiffuseCell> aCell =
          std::make_shared<CPerforatedCell>(t_Material, aCellDescription);
        return std::make_shared<CUniformDiffuseBSDFLayer>(aCell, t_BSDF);
    }

    std::shared_ptr<CBSDFLayer>
      CBSDFLayerMaker::getVenetianLayer(const std::shared_ptr<CMaterial> & t_Material,
                                        const CBSDFHemisphere & t_BSDF,
                                        double slatWidth,
                                        double slatSpacing,
                                        double slatTiltAngle,
                                        double curvatureRadius,
                                        size_t numOfSlatSegments,
                                        DistributionMethod method,
                                        const bool isHorizontal)
    {
        std::shared_ptr<ICellDescription> aCellDescription =
          std::make_shared<CVenetianCellDescription>(
            slatWidth, slatSpacing, slatTiltAngle, curvatureRadius, numOfSlatSegments);

        static const auto horizontalVenetianRotation{0.0};
        static const auto verticalVenetianRotation{90.0};
        const auto rotation{isHorizontal ? horizontalVenetianRotation : verticalVenetianRotation};

        if(method == DistributionMethod::UniformDiffuse)
        {
            std::shared_ptr<CUniformDiffuseCell> aCell =
              std::make_shared<CVenetianCell>(t_Material, aCellDescription, rotation);
            return std::make_shared<CUniformDiffuseBSDFLayer>(aCell, t_BSDF);
        }
        else
        {
            std::shared_ptr<CDirectionalDiffuseCell> aCell =
              std::make_shared<CVenetianCell>(t_Material, aCellDescription);
            return std::make_shared<CDirectionalDiffuseBSDFLayer>(aCell, t_BSDF);
        }
    }

    std::shared_ptr<CBSDFLayer>
      CBSDFLayerMaker::getPerfectlyDiffuseLayer(const std::shared_ptr<CMaterial> & t_Material,
                                                const CBSDFHemisphere & t_BSDF)
    {
        auto aDescription = std::make_shared<CFlatCellDescription>();
        auto aCell = std::make_shared<CUniformDiffuseCell>(t_Material, aDescription);
        return std::make_shared<CUniformDiffuseBSDFLayer>(aCell, t_BSDF);
    }

    std::shared_ptr<CBSDFLayer>
      CBSDFLayerMaker::getDirectionalDiffuseLayer(const std::shared_ptr<CMaterial> & t_Material,
                                                  const CBSDFHemisphere & t_BSDF)
    {
        auto aDescription = std::make_shared<CFlatCellDescription>();
        auto aCell = std::make_shared<CDirectionalDiffuseCell>(t_Material, aDescription);
        return std::make_shared<CDirectionalDiffuseBSDFLayer>(aCell, t_BSDF);
    }

    std::shared_ptr<CBSDFLayer>
      CBSDFLayerMaker::getPreLoadedBSDFLayer(const std::shared_ptr<CMaterial> & t_Material,
                                             const CBSDFHemisphere & t_BSDF)
    {
        auto aDescription = std::make_shared<CFlatCellDescription>();
        auto aCell = std::make_shared<CDirectionalDiffuseCell>(t_Material, aDescription);
        return std::make_shared<CMatrixBSDFLayer>(aCell, t_BSDF);
    }

    std::shared_ptr<CBSDFLayer>
      CBSDFLayerMaker::getWovenLayer(const std::shared_ptr<CMaterial> & t_Material,
                                     const CBSDFHemisphere & t_BSDF,
                                     double diameter,
                                     double spacing)
    {
        auto aDescription = std::make_shared<CWovenCellDescription>(diameter, spacing);
        auto aCell = std::make_shared<CWovenCell>(t_Material, aDescription);
        return std::make_shared<CUniformDiffuseBSDFLayer>(aCell, t_BSDF);
    }

    CBSDFLayerMaker::CBSDFLayerMaker(const std::shared_ptr<CMaterial> & t_Material,
                                     const CBSDFHemisphere & t_BSDF,
                                     std::shared_ptr<ICellDescription> t_Description,
                                     const DistributionMethod t_Method) :
        m_Cell(nullptr)
    {
        if(t_Material == nullptr)
        {
            throw std::runtime_error("Material for BSDF layer must be defined.");
        }

        // Specular BSDF layer is considered to be default. Default is used if t_Description is null
        // pointer
        if(t_Description == nullptr)
        {
            t_Description = std::make_shared<CSpecularCellDescription>();
        }

        // Specular cell
        if(std::dynamic_pointer_cast<CSpecularCellDescription>(t_Description) != nullptr)
        {
            m_Layer = getSpecularLayer(t_Material, t_BSDF);
        }

        if(std::dynamic_pointer_cast<CFlatCellDescription>(t_Description) != nullptr)
        {
            m_Layer = getPerfectlyDiffuseLayer(t_Material, t_BSDF);
        }

        // Venetian cell
        if(std::dynamic_pointer_cast<CVenetianCellDescription>(t_Description) != nullptr)
        {
            const auto description{
              std::dynamic_pointer_cast<CVenetianCellDescription>(t_Description)};
            m_Layer = getVenetianLayer(t_Material,
                                       t_BSDF,
                                       description->slatWidth(),
                                       description->slatSpacing(),
                                       description->slatSpacing(),
                                       description->curvatureRadius(),
                                       description->numberOfSegments(),
                                       t_Method,
                                       0);
        }

        // Perforated cell
        if(std::dynamic_pointer_cast<CCircularCellDescription>(t_Description) != nullptr)
        {
            const auto description{
              std::dynamic_pointer_cast<CCircularCellDescription>(t_Description)};
            m_Layer = getCircularPerforatedLayer(t_Material,
                                                 t_BSDF,
                                                 description->xDimension(),
                                                 description->yDimension(),
                                                 description->thickness(),
                                                 description->radius());
        }

        if(std::dynamic_pointer_cast<CRectangularCellDescription>(t_Description) != nullptr)
        {
            const auto description{
              std::dynamic_pointer_cast<CRectangularCellDescription>(t_Description)};
            m_Layer = getRectangularPerforatedLayer(t_Material,
                                                    t_BSDF,
                                                    description->xDimension(),
                                                    description->yDimension(),
                                                    description->thickness(),
                                                    description->xHole(),
                                                    description->yHole());
        }

        // Woven cell
        if(std::dynamic_pointer_cast<CWovenCellDescription>(t_Description) != nullptr)
        {
            // Woven shades do not work with directional diffuse algorithm
            const auto description{std::dynamic_pointer_cast<CWovenCellDescription>(t_Description)};
            m_Layer =
              getWovenLayer(t_Material, t_BSDF, description->diameter(), description->spacing());
        }
    }

    std::shared_ptr<CBSDFLayer> CBSDFLayerMaker::getLayer() const
    {
        return m_Layer;
    }

    std::shared_ptr<CBaseCell> CBSDFLayerMaker::getCell() const
    {
        return m_Cell;
    }
}   // namespace SingleLayerOptics
