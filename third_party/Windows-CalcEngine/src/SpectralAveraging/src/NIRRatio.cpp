#include "NIRRatio.hpp"
#include "WCECommon.hpp"


using namespace FenestrationCommon;

namespace SpectralAveraging
{
    CNIRRatio::CNIRRatio(const CSeries & t_SolarRadiation,
                         double const lowLambda,
                         double const highLambda)
    {
        auto integratedSolar = t_SolarRadiation.integrate(IntegrationType::Trapezoidal);
        auto aSolarRange = CWavelengthRange(WavelengthRange::Solar);

        auto totSolar = integratedSolar->sum(aSolarRange.minLambda(), aSolarRange.maxLambda());

        auto totVisible = integratedSolar->sum(lowLambda, highLambda);
        m_Ratio = totVisible / totSolar;
    }

    double CNIRRatio::ratio() const
    {
        return m_Ratio;
    }

}   // namespace SpectralAveraging
