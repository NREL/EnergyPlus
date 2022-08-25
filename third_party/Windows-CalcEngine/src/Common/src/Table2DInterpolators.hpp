#pragma once
#include <utility>
#include <vector>
#include <optional>

#include "Table2D.hpp"

namespace Table
{
    struct point
    {
        point() = default;
        point(std::optional<double> x, std::optional<double> y);
        bool operator==(const point & rhs) const;
        bool operator!=(const point & rhs) const;
        bool has_value() const;
        std::optional<double> x;
        std::optional<double> y;
    };

    //! Function to interpolate table data over columns for the given value
    //! It is using only linear interpolation.
    //!
    //! \param table Table that contains the data over the x and y range
    //! \param value Value that will be used to interpolate over.
    //! \return vector of points that are interpolated from a given table
    [[nodiscard]] std::vector<point>
      columnInterpolation(const Table2D<std::optional<double>> & table, double value);

    enum class Extrapolate {No, Yes};

    //! Function to interpolate point on x-y curve.
    //!
    //! \param table Table of input points (x-y)
    //! \param value Value of x for which interpolation is performed
    //! \return Interpolated y value
    [[nodiscard]] std::optional<double> tableColumnInterpolation(const std::vector<point> & table,
                                                                 double value,
                                                                 Extrapolate extrapolate = Extrapolate::No);

}   // namespace Table
