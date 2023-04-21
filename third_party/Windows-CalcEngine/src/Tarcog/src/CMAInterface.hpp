#pragma once

#include <cassert>
#include <map>

#include <WCECommon.hpp>

#include "WholeWindowConfigurations.hpp"

namespace CMA
{
    enum class Option
    {
        Best,
        Worst
    };

    class EnumOption : public FenestrationCommon::Enum<Option>
    {};

    inline EnumOption::Iterator begin(EnumOption)
    {
        return EnumOption::Iterator(static_cast<int>(Option::Best));
    }

    inline EnumOption::Iterator end(EnumOption)
    {
        return EnumOption::Iterator(static_cast<int>(Option::Worst) + 1);
    }

    template<typename T>
    class BestWorst
    {
    public:
        BestWorst(T valueBest, T valueWorst) :
            m_Value({{Option::Best, valueBest}, {Option::Worst, valueWorst}}){}

              [[nodiscard]] T value(Option option) const
        {
            assert(m_Value.count(option) != 0);
            return m_Value.at(option);
        }

    private:
        std::map<Option, T> m_Value;
    };

    class ICMAWindow
    {
    public:
        virtual ~ICMAWindow() = default;

        [[nodiscard]] virtual double uValue(double Ucog, double keffSpacer) = 0;
        [[nodiscard]] virtual double shgc(double SHGCcog, double keffSpacer) = 0;
        [[nodiscard]] virtual double vt(double tVis) = 0;
        [[nodiscard]] virtual Tarcog::IGUDimensions getIGUDimensions() = 0;
    };
}   // namespace CMA
