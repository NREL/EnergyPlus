#include <vector>
#include <iterator>
#include <cassert>
#include <stdexcept>

#include "MultiPaneSampleData.hpp"
#include "EquivalentLayerSingleComponentMW.hpp"
#include "AbsorptancesMultiPane.hpp"
#include "WCECommon.hpp"

using namespace FenestrationCommon;
using namespace SpectralAveraging;

namespace MultiLayerOptics
{
    CMultiPaneSampleData::CMultiPaneSampleData() : CSpectralSampleData()
    {}

    std::vector<double> CMultiPaneSampleData::getWavelengths() const
    {
        CCommonWavelengths aWavelengths;

        for(auto it = m_MeasuredSamples.begin(); it < m_MeasuredSamples.end(); ++it)
        {
            aWavelengths.addWavelength((*it)->getWavelengths());
        }

        // wavelengths will be combined into common one and no extrapolation will be done
        return aWavelengths.getCombinedWavelengths(Combine::Interpolate);
    }

    size_t CMultiPaneSampleData::numberOfLayers() const
    {
        return m_MeasuredSamples.size();
    }

    void CMultiPaneSampleData::addSample(const std::shared_ptr<CSpectralSampleData> & t_Sample)
    {
        m_MeasuredSamples.push_back(t_Sample);
    }

    void CMultiPaneSampleData::calculateProperties()
    {
        if(!m_absCalculated)
        {
            calculateEquivalentProperties();
            m_absCalculated = true;
        }
    }

    CSeries CMultiPaneSampleData::getLayerAbsorptances(size_t const Index)
    {
        calculateProperties();
        if((Index - 1) > m_LayerAbsorptances.size())
        {
            throw std::runtime_error("Index out of range. ");
        }
        return m_LayerAbsorptances[Index - 1];
    }

    // Interpolate current sample data to new wavelengths set
    void CMultiPaneSampleData::interpolate(const std::vector<double> & t_Wavelengths)
    {
        std::vector<std::shared_ptr<CSpectralSampleData>>::iterator it;
        for(it = m_MeasuredSamples.begin(); it < m_MeasuredSamples.end(); ++it)
        {
            (*it)->interpolate(t_Wavelengths);
        }

        CSpectralSampleData::interpolate(t_Wavelengths);
    }

    void CMultiPaneSampleData::calculateEquivalentProperties()
    {
        std::vector<double> wavelengths = getWavelengths();
        interpolate(wavelengths);

        assert(m_MeasuredSamples.size() != 0);

        CSeries Tf = m_MeasuredSamples[0]->properties(Property ::T, Side::Front);
        CSeries Tb = m_MeasuredSamples[0]->properties(Property ::T, Side::Back);
        CSeries Rf = m_MeasuredSamples[0]->properties(Property::R, Side::Front);
        CSeries Rb = m_MeasuredSamples[0]->properties(Property::R, Side::Back);
        CEquivalentLayerSingleComponentMW aEqivalentLayer(Tf, Tb, Rf, Rb);
        CAbsorptancesMultiPane aAbsorptances(Tf, Rf, Rb);

        std::vector<std::shared_ptr<CSpectralSampleData>>::iterator it;
        for(it = next(m_MeasuredSamples.begin()); it < m_MeasuredSamples.end(); ++it)
        {
            aEqivalentLayer.addLayer((*it)->properties(Property ::T, Side::Front),
                                     (*it)->properties(Property::T, Side::Back),
                                     (*it)->properties(Property ::R, Side::Front),
                                     (*it)->properties(Property ::R, Side::Back));
            aAbsorptances.addLayer((*it)->properties(Property ::T, Side::Front),
                                   (*it)->properties(Property ::R, Side::Front),
                                   (*it)->properties(Property ::R, Side::Back));
        }

        for(const auto & prop : EnumProperty())
        {
            for(const auto & side : EnumSide())
            {
                m_Property[std::make_pair(prop, side)] = aEqivalentLayer.getProperties(prop, side);
            }
        }

        m_LayerAbsorptances.clear();
        size_t size = aAbsorptances.numOfLayers();
        for(size_t i = 0; i < size; ++i)
        {
            m_LayerAbsorptances.push_back(aAbsorptances.Abs(i));
        }
    }

}   // namespace MultiLayerOptics
