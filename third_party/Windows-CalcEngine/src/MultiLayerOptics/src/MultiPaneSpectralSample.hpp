#ifndef MultiPaneSpectralSample_H
#define MultiPaneSpectralSample_H

#include <memory>
#include <vector>

#include "WCESpectralAveraging.hpp"

namespace FenestrationCommon
{
    class CSeries;
}

namespace SpectralAveraging
{
    class CSpectralSampleData;
    // class CSample;
}   // namespace SpectralAveraging

namespace MultiLayerOptics
{
    class CMultiPaneSampleData;

    class CMultiPaneSpectralSample : public SpectralAveraging::CSpectralSample
    {
    public:
        CMultiPaneSpectralSample(
          const std::shared_ptr<SpectralAveraging::CSpectralSampleData> & t_SampleData,
          const FenestrationCommon::CSeries & t_SourceData);

        double getLayerAbsorbedEnergy(double const minLambda,
                                      double const maxLambda,
                                      size_t const Index);
        double
          getLayerAbsorptance(double const minLambda, double const maxLambda, size_t const Index);

    private:
        void reset();
        void calculateProperties();
        void integrateAndAppendAbsorptances(const FenestrationCommon::CSeries & t_Absorptances);

        std::vector<FenestrationCommon::CSeries> m_AbsorbedLayersSource;
    };

}   // namespace MultiLayerOptics

#endif
