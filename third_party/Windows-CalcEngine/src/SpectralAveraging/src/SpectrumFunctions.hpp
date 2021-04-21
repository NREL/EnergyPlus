#ifndef WCESPECTRUMFUNCTIONS_H
#define WCESPECTRUMFUNCTIONS_H

#include "WCECommon.hpp"
#include <vector>

namespace SpectralAveraging
{
    /// Input wavelengths are in micrometers
    std::vector<std::pair<double, double>>
      UVAction(const std::vector<double> & t_data, double a = 3.6, double b = 12.0);

    /// Input wavelengths are in micrometers
    std::vector<std::pair<double, double>> Krochmann(const std::vector<double> & t_data);

    /// Input wavelengths are in micrometers
    std::vector<std::pair<double, double>>
      BlackBodySpectrum(const std::vector<double> & t_data, double t_temperature);
}   // namespace SpectralAveraging

#endif