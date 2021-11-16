#pragma once

#include <map>
#include <WCECommon.hpp>

namespace FenestrationCommon
{
    enum class WavelengthRange;

    /////////////////////////////////////////////////////////////////////////////////////////////
    ////   WavelengthRangeData
    /////////////////////////////////////////////////////////////////////////////////////////////

    //! \brief holds start and end wavelength value. Used to return values for range wavelengths.
    struct WavelengthRangeData
    {
        WavelengthRangeData(double startLambda, double endLambda);

        double startLambda{0};
        double endLambda{0};
    };

    //! \brief Creates wavelength range for certain pre-defined ranges give by enumerator.
    class CWavelengthRange
    {
    public:
        explicit CWavelengthRange(const WavelengthRange t_Range);
        double minLambda() const;
        double maxLambda() const;

    private:
        void setWavelengthRange(const WavelengthRange t_Range);
        double m_MinLambda;
        double m_MaxLambda;
        const std::map<WavelengthRange, WavelengthRangeData> m_WavelengthRange{
          {WavelengthRange::IR, {5.0, 100.0}},
          {WavelengthRange::Solar, {0.3, 2.5}},
          {WavelengthRange::Visible, {0.38, 0.78}}};
    };   // namespace FenestrationCommon

}   // namespace FenestrationCommon
