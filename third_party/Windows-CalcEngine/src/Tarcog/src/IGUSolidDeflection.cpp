
#include <cmath>

#include "IGUSolidDeflection.hpp"
#include "WCECommon.hpp"
#include "Surface.hpp"
#include "TarcogConstants.hpp"

using FenestrationCommon::Side;

namespace Tarcog
{
    namespace ISO15099
    {
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////
        ///    CIGUSolidLayerDeflection
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////

        CIGUSolidLayerDeflection::CIGUSolidLayerDeflection(CIGUSolidLayer const & t_SolidLayer) :
            CIGUSolidLayer(t_SolidLayer),
            m_YoungsModulus(DeflectionConstants::YOUNGSMODULUS),
            m_PoisonRatio(DeflectionConstants::POISONRATIO),
            m_Density(MaterialConstants::GLASSDENSITY)
        {}

        CIGUSolidLayerDeflection::CIGUSolidLayerDeflection(CIGUSolidLayer const & t_SolidLayer,
                                                           double const t_YoungsModulus,
                                                           double const t_PoisonRatio,
                                                           double const t_Density) :
            CIGUSolidLayer(t_SolidLayer),
            m_YoungsModulus(t_YoungsModulus),
            m_PoisonRatio(t_PoisonRatio),
            m_Density(t_Density)
        {}

        void CIGUSolidLayerDeflection::calculateConvectionOrConductionFlow()
        {
            CIGUSolidLayer::calculateConvectionOrConductionFlow();
        }

        double CIGUSolidLayerDeflection::flexuralRigidity() const
        {
            return m_YoungsModulus * pow(m_Thickness, 3) / (12 * (1 - pow(m_PoisonRatio, 2)));
        }

        std::shared_ptr<CBaseLayer> CIGUSolidLayerDeflection::clone() const
        {
            return std::make_shared<CIGUSolidLayerDeflection>(*this);
        }

        double CIGUSolidLayerDeflection::youngsModulus() const
        {
            return m_YoungsModulus;
        }

        double CIGUSolidLayerDeflection::pressureDifference() const
        {
            auto P1 = std::dynamic_pointer_cast<CGasLayer>(m_NextLayer)->getPressure();
            auto P2 = std::dynamic_pointer_cast<CGasLayer>(m_PreviousLayer)->getPressure();
            return P1 - P2;
        }

        bool CIGUSolidLayerDeflection::isDeflected() const
        {
            return true;
        }

        double CIGUSolidLayerDeflection::density() const
        {
            return m_Density;
        }

        ////////////////////////////////////////////////////////////////////////////////////////////////////////////
        ////    CIGUDeflectionTempAndPressure
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////

        CIGUDeflectionTempAndPressure::CIGUDeflectionTempAndPressure(
          std::shared_ptr<CIGUSolidLayerDeflection> const & t_SolidLayer,
          double const t_MaxDeflectionCoeff,
          double const t_MeanDeflectionCoeff) :
            CIGUSolidLayerDeflection(*t_SolidLayer),
            m_MaxCoeff(t_MaxDeflectionCoeff),
            m_MeanCoeff(t_MeanDeflectionCoeff)
        {}

        void CIGUDeflectionTempAndPressure::calculateConvectionOrConductionFlow()
        {
            CIGUSolidLayerDeflection::calculateConvectionOrConductionFlow();
            // Relaxation parameter is low because that will make possible solution to converge.
            // Instability in rest of equation is great if using higher relaxation parameter and
            // it probaby does not matter what solver is used.
            auto const RelaxationParamter = 0.005;

            auto Dp = pressureDifference();
            auto D = flexuralRigidity();
            auto Ld = m_Surface[Side::Front]->getMeanDeflection();
            Ld += LdMean(Dp, D) * RelaxationParamter;
            auto Ldmax = m_Surface[Side::Front]->getMaxDeflection();
            Ldmax += LdMax(Dp, D) * RelaxationParamter;
            for(auto aSide : FenestrationCommon::EnumSide())
            {
                m_Surface[aSide]->applyDeflection(Ld, Ldmax);
            }
        }

        double CIGUDeflectionTempAndPressure::LdMean(double const t_P, double const t_D) const
        {
            return m_MeanCoeff * t_P / t_D;
        }

        double CIGUDeflectionTempAndPressure::LdMax(double const t_P, double const t_D) const
        {
            return m_MaxCoeff * t_P / t_D;
        }

        std::shared_ptr<CBaseLayer> CIGUDeflectionTempAndPressure::clone() const
        {
            return std::make_shared<CIGUDeflectionTempAndPressure>(*this);
        }

        ////////////////////////////////////////////////////////////////////////////////////////////////////////////
        ///    CIGUDeflectionTempAndPressure
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////
        CIGUDeflectionMeasuread::CIGUDeflectionMeasuread(
          std::shared_ptr<CIGUSolidLayerDeflection> & t_Layer,
          const double t_MeanDeflection,
          const double t_MaxDeflection) :
            CIGUSolidLayerDeflection(*t_Layer)
        {
            for(Side aSide : FenestrationCommon::EnumSide())
            {
                m_Surface[aSide]->applyDeflection(t_MeanDeflection, t_MaxDeflection);
            }
        }

    }   // namespace ISO15099

}   // namespace Tarcog
