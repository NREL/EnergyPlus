
#include <cmath>

#include "WCECommon.hpp"
#include "BSDFPatch.hpp"
#include "BeamDirection.hpp"

namespace SingleLayerOptics
{
    /////////////////////////////////////////////////////////////////
    ///  CAngleLimits
    /////////////////////////////////////////////////////////////////

    CAngleLimits::CAngleLimits(double const t_Low, double const t_High) :
        m_Low(t_Low),
        m_High(t_High)
    {}

    double CAngleLimits::low() const
    {
        return m_Low;
    }

    double CAngleLimits::high() const
    {
        return m_High;
    }

    double CAngleLimits::delta() const
    {
        return m_High - m_Low;
    }

    bool CAngleLimits::isInLimits(const double t_Angle) const
    {
        // To assure that negative patch angles are covered as well
        double aAngle = (m_Low + 360) < t_Angle ? t_Angle - 360 : t_Angle;
        return (aAngle >= m_Low) && (aAngle <= m_High);
    }

    double CAngleLimits::average() const
    {
        return (m_Low + m_High) / 2;
    }

    /////////////////////////////////////////////////////////////////
    ///  CCentralAngleLimits
    /////////////////////////////////////////////////////////////////
    CCentralAngleLimits::CCentralAngleLimits(const double t_High) : CAngleLimits(0, t_High)
    {}

    double CCentralAngleLimits::average() const
    {
        return m_Low;
    }

    /////////////////////////////////////////////////////////////////
    ///  CBSDFPatch
    /////////////////////////////////////////////////////////////////

    CBSDFPatch::CBSDFPatch(const std::shared_ptr<CAngleLimits> & t_Theta,
                           const CAngleLimits & t_Phi) :
        m_Theta(t_Theta),
        m_Phi(t_Phi)
    {
        calculateLambda();
    }

    CBeamDirection CBSDFPatch::centerPoint() const
    {
        return CBeamDirection(m_Theta->average(), m_Phi.average());
    }

    double CBSDFPatch::lambda() const
    {
        return m_Lambda;
    }

    bool CBSDFPatch::isInPatch(const double t_Theta, const double t_Phi) const
    {
        return m_Theta->isInLimits(t_Theta) && m_Phi.isInLimits(t_Phi);
    }

    void CBSDFPatch::calculateLambda()
    {
        using ConstantsData::WCE_PI;

        const double thetaLow = m_Theta->low() * WCE_PI / 180;
        const double thetaHight = m_Theta->high() * WCE_PI / 180;
        const double deltaPhi = m_Phi.delta() * WCE_PI / 180;
        m_Lambda =
          0.5 * deltaPhi
          * (std::sin(thetaHight) * std::sin(thetaHight) - std::sin(thetaLow) * std::sin(thetaLow));
    }

}   // namespace SingleLayerOptics
