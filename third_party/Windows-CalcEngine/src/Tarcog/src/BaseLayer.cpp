#include "BaseLayer.hpp"


namespace Tarcog
{
    namespace ISO15099
    {
        CBaseLayer::CBaseLayer() :
            CState(),
            CLayerGeometry(),
            CLayerHeatFlow(),
            m_PreviousLayer(nullptr),
            m_NextLayer(nullptr)
        {}

        std::shared_ptr<CBaseLayer> CBaseLayer::getPreviousLayer() const
        {
            return m_PreviousLayer;
        }

        std::shared_ptr<CBaseLayer> CBaseLayer::getNextLayer() const
        {
            return m_NextLayer;
        }

        void CBaseLayer::tearDownConnections()
        {
            m_PreviousLayer = nullptr;
            m_NextLayer = nullptr;
        }

        void CBaseLayer::connectToBackSide(const std::shared_ptr<CBaseLayer> & t_Layer)
        {
            m_NextLayer = t_Layer;
            t_Layer->m_PreviousLayer = shared_from_this();
        }

        void CBaseLayer::calculateRadiationFlow()
        {}

        double CBaseLayer::getThickness() const
        {
            return 0;
        }

        bool CBaseLayer::isPermeable() const
        {
            return false;
        }
    }   // namespace ISO15099

}   // namespace Tarcog
