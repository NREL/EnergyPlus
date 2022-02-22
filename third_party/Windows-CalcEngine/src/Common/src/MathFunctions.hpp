#pragma once

#include <cmath>
#include <algorithm>
#include <stdexcept>

namespace FenestrationCommon
{
    static const double WCE_PI = 4.0 * std::atan(1.0);

    inline double radians(const double d)
    {
        return d * WCE_PI / 180;
    }

    inline double degrees(const double r)
    {
        return r * 180 / WCE_PI;
    }

    struct TR
    {
        double T;
        double R;
    };

    inline TR checkRange(const double T, const double R)
    {
        TR tr{T, R};
        if(T + R > 1)
        {
            // Brackets around std::max are necessary because this fails when included in MFC files
            // that uses Windows.h
            const auto RTMax = (std::max)(T, R);
            if(RTMax == R)
            {
                tr.R = 1 - T;
            }
            if(RTMax == T)
            {
                tr.T = 1 - R;
            }
        }
        return tr;
    }

    //! Finds linear interpolation between the points for a given value
    inline double linearInterpolation(double x1, double x2, double y1, double y2, double x)
    {
        double delta{0};
        if(x1 != x2)
        {
            delta = ((y2 - y1) / (x2 - x1)) * (x - x1);
        }
        return y1 + delta;
    }
}   // namespace FenestrationCommon
