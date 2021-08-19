#include <iterator>
#include <stdexcept>

#include "MultiPaneSpectralSample.hpp"
#include "MultiPaneSampleData.hpp"
#include "WCESpectralAveraging.hpp"
#include "WCECommon.hpp"

using namespace SpectralAveraging;
using namespace FenestrationCommon;

namespace MultiLayerOptics
{
    CMultiPaneSpectralSample::CMultiPaneSpectralSample(
      const std::shared_ptr<CSpectralSampleData> & t_SampleData, const CSeries & t_SourceData) :
        CSpectralSample(t_SampleData, t_SourceData)
    {}

    double CMultiPaneSpectralSample::getLayerAbsorbedEnergy(double const minLambda,
                                                            double const maxLambda,
                                                            size_t const Index)
    {
        double aEnergy = 0;
        calculateState();
        if(Index > m_AbsorbedLayersSource.size())
        {
            throw std::runtime_error("Index for glazing layer absorptance is out of range.");
        }
        aEnergy = m_AbsorbedLayersSource[Index - 1].sum(minLambda, maxLambda);
        return aEnergy;
    }

    double CMultiPaneSpectralSample::getLayerAbsorptance(double const minLambda,
                                                         double const maxLambda,
                                                         size_t const Index)
    {
        calculateState();
        double absorbedEnergy = getLayerAbsorbedEnergy(minLambda, maxLambda, Index);
        double incomingEnergy = m_IncomingSource.sum(minLambda, maxLambda);
        return absorbedEnergy / incomingEnergy;
    }

    void CMultiPaneSpectralSample::calculateProperties()
    {
        if(!m_StateCalculated)
        {
            CSpectralSample::calculateProperties();
            if(std::dynamic_pointer_cast<CMultiPaneSampleData>(m_SampleData) != NULL)
            {
                std::shared_ptr<CMultiPaneSampleData> aSample =
                  std::dynamic_pointer_cast<CMultiPaneSampleData>(m_SampleData);
                size_t numOfLayers = aSample->numberOfLayers();
                for(size_t i = 0; i < numOfLayers; ++i)
                {
                    auto layerAbsorbed = aSample->getLayerAbsorptances(i + 1);
                    integrateAndAppendAbsorptances(layerAbsorbed);
                }
            }
            else
            {
                // Perspective is always from front side when using in MultiLayerOptics. Flipping
                // flag should be used when putting layer in IGU
                auto layerAbsorbed = m_SampleData->properties(Property ::Abs, Side::Front);
                integrateAndAppendAbsorptances(layerAbsorbed);
            }

            m_StateCalculated = true;
        }
    }

    void CMultiPaneSpectralSample::integrateAndAppendAbsorptances(const CSeries & t_Absorptances)
    {
        CSeries aAbs = t_Absorptances;
        if(m_WavelengthSet != WavelengthSet::Data)
        {
            aAbs = aAbs.interpolate(m_Wavelengths);
        }
        aAbs = aAbs * m_IncomingSource;
        aAbs = *aAbs.integrate(m_IntegrationType, m_NormalizationCoefficient);
        m_AbsorbedLayersSource.push_back(aAbs);
    }

    void CMultiPaneSpectralSample::reset()
    {
        CSpectralSample::reset();
        m_AbsorbedLayersSource.clear();
    }

}   // namespace MultiLayerOptics
