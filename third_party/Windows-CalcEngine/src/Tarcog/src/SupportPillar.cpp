
#include <cmath>
#include <memory>

#include "WCETarcog.hpp"


namespace Tarcog
{
    namespace ISO15099
    {
        ////////////////////////////////////////////////////////////////////////////
        ////  CSupportPillar
        ////////////////////////////////////////////////////////////////////////////
        CSupportPillar::CSupportPillar(CIGUGapLayer const & t_Layer, double const t_Conductivity) :
            CIGUGapLayer(t_Layer),
            m_Conductivity(t_Conductivity)
        {}

        void CSupportPillar::calculateConvectionOrConductionFlow()
        {
            CIGUGapLayer::calculateConvectionOrConductionFlow();
            if(!isCalculated())
            {
                m_ConductiveConvectiveCoeff += conductivityOfPillarArray();
            }
        }

        ////////////////////////////////////////////////////////////////////////////
        ////  CCircularPillar
        ////////////////////////////////////////////////////////////////////////////
        CCircularPillar::CCircularPillar(CIGUGapLayer const & t_Gap,
                                         double const t_Conductivity,
                                         double const t_Spacing,
                                         double const t_Radius) :
            CSupportPillar(t_Gap, t_Conductivity),
            m_Spacing(t_Spacing),
            m_Radius(t_Radius)
        {}

        double CCircularPillar::conductivityOfPillarArray()
        {
            using ConstantsData::WCE_PI;

            auto cond1 =
              std::dynamic_pointer_cast<CBaseIGULayer>(m_PreviousLayer)->getConductivity();
            auto cond2 = std::dynamic_pointer_cast<CBaseIGULayer>(m_NextLayer)->getConductivity();
            auto aveCond = (cond1 + cond2) / 2;

            auto cond = 2 * aveCond * m_Radius / (pow(m_Spacing, 2));
            cond *= 1 / (1 + 2 * m_Thickness * aveCond / (m_Conductivity * WCE_PI * m_Radius));

            return cond;
        }

        std::shared_ptr<Tarcog::ISO15099::CBaseLayer> CCircularPillar::clone() const
        {
            return std::make_shared<CCircularPillar>(*this);
        }

    }   // namespace ISO15099

}   // namespace Tarcog
