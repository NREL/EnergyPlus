#include "PhotovoltaicLayer.hpp"

#include <utility>
#include "MaterialDescription.hpp"

namespace SingleLayerOptics
{
    ///////////////////////////////////////////////////////////////////////////
    /// PVPowerProperties
    ///////////////////////////////////////////////////////////////////////////
    PVPowerProperties::PVPowerProperties(double jsc, double voc, double ff) :
        JSC(jsc), VOC(voc), FF(ff)
    {}

    ///////////////////////////////////////////////////////////////////////////
    /// PVPowerPropertiesTable
    ///////////////////////////////////////////////////////////////////////////
    PVPowerPropertiesTable::PVPowerPropertiesTable(
      std::vector<PVPowerProperties> pvPowerProperties) :
        m_PVPowerProperties(std::move(pvPowerProperties))
    {}

    double PVPowerPropertiesTable::voc(const double jsc) const
    {
        const auto el{jsc / 10};   // Need to convert from standard SI into what user provided
        const auto ind{findIndexes(el)};
        const auto value{FenestrationCommon::linearInterpolation(m_PVPowerProperties[ind.first].JSC,
                                                                 m_PVPowerProperties[ind.second].JSC,
                                                                 m_PVPowerProperties[ind.first].VOC,
                                                                 m_PVPowerProperties[ind.second].VOC,
                                                                 el)};

        return value;
    }

    double PVPowerPropertiesTable::ff(double jsc) const
    {
        const auto el{jsc / 10};   // Need to convert from standard SI into what user provided
        const auto ind{findIndexes(el)};
        const auto value{FenestrationCommon::linearInterpolation(m_PVPowerProperties[ind.first].JSC,
                                                                 m_PVPowerProperties[ind.second].JSC,
                                                                 m_PVPowerProperties[ind.first].FF,
                                                                 m_PVPowerProperties[ind.second].FF,
                                                                 el)};

        return value;
    }

    PVPowerPropertiesTable::SearchIndexes PVPowerPropertiesTable::findIndexes(const double el) const
    {
        size_t index{0u};
        for(size_t i = 0u; i < m_PVPowerProperties.size(); ++i)
        {
            if(el > m_PVPowerProperties[i].JSC)
            {
                index = i;
            }
        }

        const auto lastIndex{index == m_PVPowerProperties.size() ? index : index + 1u};
        return {index, lastIndex};
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////
    // PhotovoltaicLayer
    ////////////////////////////////////////////////////////////////////////////////////////////////

    PhotovoltaicLayer::PhotovoltaicLayer(const CSpecularCell & cell,
                                         const std::shared_ptr<CMaterialPhotovoltaic> & material) :
        SpecularLayer(cell), m_PVMaterial(material)
    {}

    void PhotovoltaicLayer::assignPowerTable(PVPowerPropertiesTable powerTable)
    {
        m_PVPowerTable = std::move(powerTable);
    }

    double PhotovoltaicLayer::voc(double electricity) const
    {
        return m_PVPowerTable.voc(electricity);
    }

    double PhotovoltaicLayer::ff(double electricity) const
    {
        return m_PVPowerTable.ff(electricity);
    }

    std::shared_ptr<PhotovoltaicLayer>
      PhotovoltaicLayer::createLayer(const std::shared_ptr<CMaterialPhotovoltaic> & material, PVPowerPropertiesTable powerTable)
    {
        auto aCell = CSpecularCell(material);
        auto layer{std::make_shared<PhotovoltaicLayer>(aCell, material)};

        layer->assignPowerTable(powerTable);

        return layer;
    }

    FenestrationCommon::CSeries PhotovoltaicLayer::jscPrime(FenestrationCommon::Side t_Side) const
    {
        return m_PVMaterial->jscPrime(t_Side);
    }
}   // namespace SingleLayerOptics