#include <cassert>

#include "BaseCell.hpp"
#include "CellDescription.hpp"
#include "WCECommon.hpp"
#include "MaterialDescription.hpp"

using namespace FenestrationCommon;

namespace SingleLayerOptics
{
    CBaseCell::CBaseCell() : m_Material(nullptr), m_CellDescription(nullptr)
    {}

    CBaseCell::CBaseCell(const std::shared_ptr<CMaterial> & t_Material,
                         const std::shared_ptr<ICellDescription> & t_CellDescription) :
        m_Material(t_Material),
        m_CellDescription(t_CellDescription)
    {}

    void CBaseCell::setSourceData(CSeries & t_SourceData)
    {
        m_Material->setSourceData(t_SourceData);
    }

    double CBaseCell::T_dir_dir(const Side t_Side, const CBeamDirection & t_Direction)
    {
        return m_CellDescription->T_dir_dir(t_Side, t_Direction);
    }

    double CBaseCell::R_dir_dir(const Side t_Side, const CBeamDirection & t_Direction)
    {
        return m_CellDescription->R_dir_dir(t_Side, t_Direction);
    }

    std::vector<double> CBaseCell::T_dir_dir_band(const Side t_Side,
                                                  const CBeamDirection & t_Direction)
    {
        const double value = T_dir_dir(t_Side, t_Direction);
        std::vector<double> aResults;
        std::vector<double> aMaterials = m_Material->getBandProperties(Property::T, t_Side);
        size_t size = aMaterials.size();
        for(size_t i = 0; i < size; i++)
        {
            aResults.push_back(value);
        }

        return aResults;
    }

    std::vector<double> CBaseCell::R_dir_dir_band(const Side t_Side,
                                                  const CBeamDirection & t_Direction)
    {
        double value = R_dir_dir(t_Side, t_Direction);
        std::vector<double> aResults;
        std::vector<double> aMaterials = m_Material->getBandProperties(Property::R, t_Side);
        size_t size = aMaterials.size();
        for(size_t i = 0; i < size; i++)
        {
            aResults.push_back(value);
        }

        return aResults;
    }

    std::vector<double> CBaseCell::getBandWavelengths() const
    {
        assert(m_Material != nullptr);
        return m_Material->getBandWavelengths();
    }

    void CBaseCell::setBandWavelengths(const std::vector<double> & wavelengths) const
    {
        assert(m_Material != nullptr);
        m_Material->setBandWavelengths(wavelengths);
    }

    int CBaseCell::getBandIndex(const double t_Wavelength) const
    {
        return m_Material->getBandIndex(t_Wavelength);
    }

    size_t CBaseCell::getBandSize() const
    {
        return m_Material->getBandSize();
    }

    double CBaseCell::getMinLambda() const
    {
        return m_Material->getMinLambda();
    }

    double CBaseCell::getMaxLambda() const
    {
        return m_Material->getMaxLambda();
    }

    void CBaseCell::Flipped(bool flipped) const
    {
        m_Material->Flipped(flipped);
    }

}   // namespace SingleLayerOptics
