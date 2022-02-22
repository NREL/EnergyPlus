#include <algorithm>
#include <optional>

#include "Table2DInterpolators.hpp"

namespace Table
{
    //! Intentionally not visible in the header file
    std::optional<double> linearInterpolation(std::optional<double> x1,
                                              std::optional<double> x2,
                                              std::optional<double> y1,
                                              std::optional<double> y2,
                                              double x)
    {
        if(x1.has_value() && x2.has_value() && y1.has_value() && y2.has_value())
        {
            return x1.value() != x2.value()
                     ? y1.value()
                         + (y2.value() - y1.value()) / (x2.value() - x1.value()) * (x - x1.value())
                     : y1.value();
        }
        else
        {
            return std::optional<double>();
        }
    }

    point::point(std::optional<double> x, std::optional<double> y) :
        x(std::move(x)), y(std::move(y))
    {}

    bool point::operator==(const point & rhs) const
    {
        const auto x_has_value{x.has_value() == rhs.x.has_value()};
        const auto y_has_value{y.has_value() == rhs.y.has_value()};
        auto result{x_has_value && y_has_value};
        if(x_has_value)
        {
            result = result && x.value() == rhs.x.value();
        }
        if(y_has_value)
        {
            result = result && y.value() == rhs.y.value();
        }
        return result;
    }

    bool point::operator!=(const point & rhs) const
    {
        return !(rhs == *this);
    }

    bool point::has_value() const
    {
        return x.has_value() && y.has_value();
    }

    std::vector<point> columnInterpolation(const Table2D<std::optional<double>> & table,
                                           double value)
    {
        const auto x{table.x_values()};
        const auto lowerIt{std::lower_bound(x.begin(), x.end(), value)};
        const auto upperIndex{lowerIt - x.begin()};
        const auto lowIndex(upperIndex == 0u ? 0u : upperIndex - 1u);

        const auto lowerV{table.column(lowIndex)};
        const auto upperV{table.column(upperIndex)};

        const auto y{table.y_values()};

        std::vector<point> result;

        for(size_t i = 0u; i < y.size(); ++i)
        {
            const auto val{
              linearInterpolation(x[lowIndex], x[upperIndex], lowerV[i], upperV[i], value)};
            result.emplace_back(y[i], val);
        }

        return result;
    }

    std::optional<double> tableColumnInterpolation(const std::vector<point> & table,
                                                   double value,
                                                   Extrapolate extrapolate)
    {
        std::optional<double> result;
        point p1;
        point p2;

        bool ptFound{false};
        for(const auto & pt : table)
        {
            if(pt.x > value)
            {
                ptFound = true;
            }

            if(!ptFound)
            {
                p1 = pt;
            }
            else
            {
                p2 = pt;
                break;
            }
        }

        if(p1.has_value() && p2.has_value() && p1 != p2)
        {
            result = linearInterpolation(p1.x, p2.x, p1.y, p2.y, value);
        }

        if(!result.has_value() && extrapolate == Extrapolate::Yes)
        {
            const auto firstValue{
              std::find_if(std::begin(table), std::end(table), [&](const point & a) {
                  return a.y.has_value();
              })};
            const auto lastValue{
              std::find_if(std::rbegin(table), std::rend(table), [&](const point & a) {
                  return a.y.has_value();
              })};
            result = table[0].x.value() > value ? firstValue->y : lastValue->y;
        }

        return result;
    }
}   // namespace Table
