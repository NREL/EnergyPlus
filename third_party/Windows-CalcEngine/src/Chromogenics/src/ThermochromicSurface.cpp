#include "ThermochromicSurface.hpp"

namespace Chromogenics
{
    namespace ISO15099
    {
        //////////////////////////////////////////////////////////////////////////////
        /// CThermochromicSurface
        //////////////////////////////////////////////////////////////////////////////
        CThermochromicSurface::CThermochromicSurface(
          std::vector<std::pair<double, double>> const & t_Emissivity,
          std::vector<std::pair<double, double>> const & t_Transmittance) :
            ISurface(0, 0),
            m_EmissivityInterpolator(
              std::make_shared<FenestrationCommon::CSPChipInterpolation2D>(t_Emissivity)),
            m_TransmittanceInterpolator(
              std::make_shared<FenestrationCommon::CSPChipInterpolation2D>(t_Transmittance))
        {}

        CThermochromicSurface::CThermochromicSurface(
          const double t_Emissivity,
          std::vector<std::pair<double, double>> const & t_Transmittance) :
            ISurface(t_Emissivity, 0),
            m_EmissivityInterpolator(nullptr),
            m_TransmittanceInterpolator(
              std::make_shared<FenestrationCommon::CSPChipInterpolation2D>(t_Transmittance))
        {}

        CThermochromicSurface::CThermochromicSurface(
          std::vector<std::pair<double, double>> const & t_Emissivity,
          const double t_Transmittance) :
            ISurface(0, t_Transmittance),
            m_EmissivityInterpolator(
              std::make_shared<FenestrationCommon::CSPChipInterpolation2D>(t_Emissivity)),
            m_TransmittanceInterpolator(nullptr)
        {}

        CThermochromicSurface::CThermochromicSurface(CThermochromicSurface const & t_Surface) :
            ISurface(t_Surface),
            m_EmissivityInterpolator(t_Surface.m_EmissivityInterpolator),
            m_TransmittanceInterpolator(t_Surface.m_TransmittanceInterpolator)
        {}

        CThermochromicSurface & CThermochromicSurface::
          operator=(CThermochromicSurface const & t_Surface)
        {
            m_EmissivityInterpolator = t_Surface.m_EmissivityInterpolator;
            m_TransmittanceInterpolator = t_Surface.m_TransmittanceInterpolator;
            return *this;
        }

        std::shared_ptr<Tarcog::ISO15099::ISurface> CThermochromicSurface::clone() const
        {
            return std::make_shared<CThermochromicSurface>(*this);
        }

        void CThermochromicSurface::setTemperature(double const t_Temperature)
        {
            if(m_EmissivityInterpolator != nullptr)
            {
                m_Emissivity = m_EmissivityInterpolator->getValue(t_Temperature);
                calculateReflectance();
            }
            if(m_TransmittanceInterpolator != nullptr)
            {
                m_Transmittance = m_TransmittanceInterpolator->getValue(t_Temperature);
                calculateReflectance();
            }
            m_Temperature = t_Temperature;
        }

    }   // namespace ISO15099

}   // namespace Chromogenics
