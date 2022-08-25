#pragma once

#include <memory>

#include "SpecularLayer.hpp"

namespace SingleLayerOptics
{
    class CMaterialPhotovoltaic;

    ///////////////////////////////////////////////////////////////////////////
    /// PVPowerProperties
    ///////////////////////////////////////////////////////////////////////////

    //! Simple structure to store photovoltaic power properties.
    //!
    //! Not that unit for current is not using standard SI units. The reason is that users will
    //! provide the table in this format and this will avoid any conversions.
    struct PVPowerProperties
    {
        PVPowerProperties(double jsc, double voc, double ff);

        double JSC;   // mA/cm^2
        double VOC;   // V
        double FF;    // Unitless
    };

    ///////////////////////////////////////////////////////////////////////////
    /// PhotovoltaicSampleData
    ///////////////////////////////////////////////////////////////////////////

    class PVPowerPropertiesTable
    {
    public:
        PVPowerPropertiesTable() = default;
        PVPowerPropertiesTable(std::vector<PVPowerProperties> pvPowerProperties);

        [[nodiscard]] double voc(double jsc) const;
        [[nodiscard]] double ff(double jsc) const;

    private:
        struct SearchIndexes
        {
            size_t first;
            size_t second;
        };

        SearchIndexes findIndexes(double el) const;

        std::vector<PVPowerProperties> m_PVPowerProperties;
    };

    ////////////////////////////////////////////////////////////////////////////////////////////////
    // PhotovoltaicLayer
    ////////////////////////////////////////////////////////////////////////////////////////////////

    class PhotovoltaicLayer : public SpecularLayer
    {
    public:
        static std::shared_ptr<PhotovoltaicLayer>
          createLayer(const std::shared_ptr<CMaterialPhotovoltaic> & material, PVPowerPropertiesTable powerTable);

        [[nodiscard]] FenestrationCommon::CSeries jscPrime(FenestrationCommon::Side t_Side) const;

        explicit PhotovoltaicLayer(const CSpecularCell & cell,
                                   const std::shared_ptr<CMaterialPhotovoltaic> & material);

        void assignPowerTable(PVPowerPropertiesTable powerTable);

        [[nodiscard]] double voc(double electricity) const;
        [[nodiscard]] double ff(double electricity) const;

    private:
        std::shared_ptr<CMaterialPhotovoltaic> m_PVMaterial;
        PVPowerPropertiesTable m_PVPowerTable;
    };

}   // namespace SingleLayerOptics