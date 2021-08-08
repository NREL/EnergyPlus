#include <cassert>

#include "WavelengthRange.hpp"
#include "WCECommon.hpp"


namespace FenestrationCommon
{
    /////////////////////////////////////////////////////////////////////////////////////////////
    ////   WavelengthRangeData
    /////////////////////////////////////////////////////////////////////////////////////////////

    WavelengthRangeData::WavelengthRangeData(const double startLambda, const double endLambda) :
        startLambda(startLambda),
        endLambda(endLambda)
    {}

    /////////////////////////////////////////////////////////////////////////////////////////////
    ////   CWavelengthRange
    /////////////////////////////////////////////////////////////////////////////////////////////

    CWavelengthRange::CWavelengthRange(const WavelengthRange t_Range)
    {
        setWavelengthRange(t_Range);
    }

    double CWavelengthRange::minLambda() const
    {
        return m_MinLambda;
    }

    double CWavelengthRange::maxLambda() const
    {
        return m_MaxLambda;
    }

    void CWavelengthRange::setWavelengthRange(const WavelengthRange t_Range)
    {
        const auto wRange{m_WavelengthRange.at(t_Range)};
        m_MinLambda = wRange.startLambda;
        m_MaxLambda = wRange.endLambda;
    }

}   // namespace FenestrationCommon
