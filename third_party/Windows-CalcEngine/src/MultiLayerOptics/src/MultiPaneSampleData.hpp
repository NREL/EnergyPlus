#ifndef MultiLayerOpticsMEASUREDDATA_H
#define MultiLayerOpticsMEASUREDDATA_H

#include "WCESpectralAveraging.hpp"
#include <WCECommon.hpp>

namespace MultiLayerOptics
{
    // Contain multiple spectral data samples. It also calculates MultiLayerOptics properties
    // (Transmittance, Front reflectance and back reflectance)
    class CMultiPaneSampleData : public SpectralAveraging::CSpectralSampleData
    {
    public:
        CMultiPaneSampleData();

        void addSample(const std::shared_ptr<CSpectralSampleData> & t_Sample);
        FenestrationCommon::CSeries getLayerAbsorptances(size_t const Index);
        std::vector<double> getWavelengths() const;
        size_t numberOfLayers() const;

    private:
        void calculateProperties();

        void interpolate(const std::vector<double> & t_Wavelengths);
        void calculateEquivalentProperties();

        std::vector<std::shared_ptr<CSpectralSampleData>> m_MeasuredSamples;
        std::vector<FenestrationCommon::CSeries> m_LayerAbsorptances;
    };

}   // namespace MultiLayerOptics

#endif
