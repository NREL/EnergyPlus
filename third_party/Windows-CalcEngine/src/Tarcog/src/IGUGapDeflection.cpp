#include "IGUGapDeflection.hpp"
#include "WCECommon.hpp"
#include "Surface.hpp"


namespace Tarcog
{
    namespace ISO15099
    {
        CIGUGapLayerDeflection::CIGUGapLayerDeflection(CIGUGapLayer const & t_GapLayer,
                                                       double const t_Tini,
                                                       double const t_Pini) :
            CIGUGapLayer(t_GapLayer),
            m_Tini(t_Tini),
            m_Pini(t_Pini)
        {}

        double CIGUGapLayerDeflection::getPressure()
        {
            auto Vini = m_Width * m_Height * m_Thickness;
            auto modThickness = getThickness();
            auto Vgap = m_Width * m_Height * modThickness;
            return m_Pini * Vini * layerTemperature() / (m_Tini * Vgap);
        }

        std::shared_ptr<CBaseLayer> CIGUGapLayerDeflection::clone() const
        {
            return std::make_shared<CIGUGapLayerDeflection>(*this);
        }

    }   // namespace ISO15099

}   // namespace Tarcog
