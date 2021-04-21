#pragma once

#include <vector>

namespace FenestrationCommon
{
    std::vector<double> generateSpectrum(size_t numOfVisibleBands, size_t numOfIRBands);

    std::vector<double> generateISO9050Wavelengths();
}