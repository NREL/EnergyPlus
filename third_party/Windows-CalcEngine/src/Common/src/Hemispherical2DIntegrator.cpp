#include <vector>

#include "Hemispherical2DIntegrator.hpp"
#include "Series.hpp"
#include "IntegratorStrategy.hpp"
#include "MathFunctions.hpp"


namespace FenestrationCommon
{
    // Performs hemispherical 2D integration
    CHemispherical2DIntegrator::CHemispherical2DIntegrator(const CSeries & t_Series,
                                                           const IntegrationType t_IntegrationType,
                                                           double normalizationCoefficient)
    {
        CSeries aResultValues = CSeries();
        for(const auto & ser : t_Series)
        {
            auto angle = radians(ser->x());
            auto value = ser->value();
            auto sinCos = std::sin(angle) * std::cos(angle);
            aResultValues.addProperty(angle, value * sinCos);
        }

        aResultValues.sort();

        auto integrated = aResultValues.integrate(t_IntegrationType, normalizationCoefficient);
        m_Value = 2 * integrated->sum();
    }

    double CHemispherical2DIntegrator::value() const
    {
        return m_Value;
    }

}   // namespace FenestrationCommon
