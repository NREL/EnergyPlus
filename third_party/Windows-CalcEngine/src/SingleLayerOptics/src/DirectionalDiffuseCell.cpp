#include "DirectionalDiffuseCell.hpp"
#include "MaterialDescription.hpp"

namespace SingleLayerOptics
{
    CDirectionalDiffuseCell::CDirectionalDiffuseCell(
      const std::shared_ptr<CMaterial> & t_MaterialProperties,
      const std::shared_ptr<ICellDescription> & t_Cell,
      double rotation) :
        CBaseCell(t_MaterialProperties, t_Cell, rotation)
    {}

    double CDirectionalDiffuseCell::T_dir_dif(const FenestrationCommon::Side t_Side,
                                              const CBeamDirection & t_IncomingDirection,
                                              const CBeamDirection & t_OutgoingDirection)
    {
        double cellT = CBaseCell::T_dir_dir(t_Side, t_IncomingDirection);
        double materialT = m_Material->getProperty(
          FenestrationCommon::Property::T, t_Side, t_IncomingDirection, t_OutgoingDirection);
        double t = cellT + (1 - cellT) * materialT;
        return t;
    }

    double CDirectionalDiffuseCell::R_dir_dif(const FenestrationCommon::Side t_Side,
                                              const CBeamDirection & t_IncomingDirection,
                                              const CBeamDirection & t_OutgoingDirection)
    {
        double cellT = CBaseCell::T_dir_dir(t_Side, t_IncomingDirection);
        double cellR = CBaseCell::R_dir_dir(t_Side, t_IncomingDirection);
        double materialR = m_Material->getProperty(
          FenestrationCommon::Property::R, t_Side, t_IncomingDirection, t_OutgoingDirection);
        double r = cellR + (1 - cellT) * materialR;
        return r;
    }

    std::vector<double>
      CDirectionalDiffuseCell::T_dir_dif_band(const FenestrationCommon::Side t_Side,
                                              const CBeamDirection & t_IncomingDirection,
                                              const CBeamDirection & t_OutgoingDirection)
    {
        double cellT = CBaseCell::T_dir_dir(t_Side, t_IncomingDirection);

        auto materialBandValues = m_Material->getBandProperties(
          FenestrationCommon::Property::T, t_Side, t_IncomingDirection, t_OutgoingDirection);

        std::vector<double> result;
        result.reserve(materialBandValues.size());
        for(auto materialBandValue : materialBandValues)
        {
            result.push_back(cellT + (1 - cellT) * materialBandValue);
        }

        return result;
    }

    std::vector<double>
      CDirectionalDiffuseCell::R_dir_dif_band(const FenestrationCommon::Side t_Side,
                                              const CBeamDirection & t_IncomingDirection,
                                              const CBeamDirection & t_OutgoingDirection)
    {
        double cellT = CBaseCell::T_dir_dir(t_Side, t_IncomingDirection);
        double cellR = CBaseCell::R_dir_dir(t_Side, t_IncomingDirection);

        auto materialBandValues = m_Material->getBandProperties(
          FenestrationCommon::Property::R, t_Side, t_IncomingDirection, t_OutgoingDirection);

        std::vector<double> result;
        result.reserve(materialBandValues.size());
        for(auto materialBandValue : materialBandValues)
        {
            result.push_back(cellR + (1 - cellT) * materialBandValue);
        }

        return result;
    }

}   // namespace SingleLayerOptics
