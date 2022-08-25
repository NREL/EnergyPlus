#pragma once

#include <optional>

#include <WCECommon.hpp>

namespace DeflectionData
{
    //! Hard coded data used in the deflection calculations. This table is only valid for the
    //! Poisson's ratio equal 0.22.
    Table::Table2D<std::optional<double>> getWNData();

    //! Hard coded data used in the deflection calculations. This table is only valid for the
    //! Poisson's ratio equal 0.22.
    Table::Table2D<std::optional<double>> getVNData();
}   // namespace DeflectionData
