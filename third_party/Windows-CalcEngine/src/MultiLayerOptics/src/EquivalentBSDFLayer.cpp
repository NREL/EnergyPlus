
#include <cmath>
#include <cassert>
#include <stdexcept>

#include "EquivalentBSDFLayer.hpp"
#include "EquivalentBSDFLayerSingleBand.hpp"
#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"

using namespace FenestrationCommon;
using namespace SingleLayerOptics;

namespace MultiLayerOptics
{
    CEquivalentBSDFLayer::CEquivalentBSDFLayer(const std::vector<double> & t_CommonWavelengths,
                                               const std::shared_ptr<CBSDFLayer> & t_Layer) :
        m_Lambda(t_Layer->getResults()->lambdaMatrix()),
        m_CombinedLayerWavelengths(t_CommonWavelengths),
        m_Calculated(false)
    {
        if(t_Layer == nullptr)
        {
            throw std::runtime_error("Equivalent BSDF Layer must contain valid layer.");
        }

        addLayer(t_Layer);
    }

    void CEquivalentBSDFLayer::addLayer(const std::shared_ptr<CBSDFLayer> & t_Layer)
    {
        updateWavelengthLayers(*t_Layer);
        m_Layer.push_back(t_Layer);
    }

    const CBSDFDirections & CEquivalentBSDFLayer::getDirections(const BSDFDirection t_Side) const
    {
        return m_Layer[0]->getDirections(t_Side);
    }

    std::vector<double> CEquivalentBSDFLayer::getCommonWavelengths() const
    {
        return m_CombinedLayerWavelengths;
    }

	double CEquivalentBSDFLayer::getMinLambda() const
	{
		return m_CombinedLayerWavelengths.front();
	}

	double CEquivalentBSDFLayer::getMaxLambda() const
	{
		return m_CombinedLayerWavelengths.back();
	}

    std::shared_ptr<CMatrixSeries> CEquivalentBSDFLayer::getTotalA(const Side t_Side)
    {
        if(!m_Calculated)
        {
            calculate();
        }
        return m_TotA.at(t_Side);
    }

    std::shared_ptr<CMatrixSeries> CEquivalentBSDFLayer::getTotal(const Side t_Side,
                                                                  const PropertySimple t_Property)
    {
        if(!m_Calculated)
        {
            calculate();
        }
        return m_Tot.at(std::make_pair(t_Side, t_Property));
    }

    void CEquivalentBSDFLayer::setSolarRadiation(CSeries &t_SolarRadiation)
    {
        // Need to recreate wavelenght by wavelength layers
        m_LayersWL.clear();
        for(auto & aLayer : m_Layer)
        {
            aLayer->setSourceData(t_SolarRadiation);
            updateWavelengthLayers(*aLayer);
        }
        m_Calculated = false;
    }

    void CEquivalentBSDFLayer::calculate()
    {
        size_t matrixSize = m_Lambda.size();
        size_t numberOfLayers = m_LayersWL[0].getNumberOfLayers();

        for(Side aSide : EnumSide())
        {
            m_TotA[aSide] = std::make_shared<CMatrixSeries>(numberOfLayers, matrixSize);
            for(PropertySimple aProperty : EnumPropertySimple())
            {
                m_Tot[std::make_pair(aSide, aProperty)] =
                  std::make_shared<CMatrixSeries>(matrixSize, matrixSize);
            }
        }

        // Calculate total transmitted solar per matrix and perform integration over each wavelength
        size_t WLsize = m_CombinedLayerWavelengths.size();

        // // This is for multithread calculations.
        // size_t numOfThreads = size_t( thread::hardware_concurrency() - 2 );
        // size_t step = WLsize / numOfThreads;
        // std::vector< std::shared_ptr< thread > > aThreads = std::vector< std::shared_ptr< thread
        // > >( numOfThreads );
        //
        // size_t startNum = 0;
        // size_t endNum = step;
        //
        // for( size_t i = 0; i < numOfThreads; ++i ) {
        //   if( i == numOfThreads - 1 ) {
        //     endNum = WLsize;
        //   }
        //
        //   aThreads[ i ] = std::make_shared< thread >(
        //   &CEquivalentBSDFLayer::calculateWavelengthProperties, *this,
        //    numberOfLayers, startNum, endNum );
        //
        //   startNum += step;
        //   endNum += step;
        // }
        //
        // for( size_t i = 0; i < numOfThreads; ++i ) {
        //   aThreads[ i ]->join();
        // }
        //
        // // End of multithreaded calculations.


        calculateWavelengthProperties(numberOfLayers, 0, WLsize);

        m_Calculated = true;
    }

    void CEquivalentBSDFLayer::calculateWavelengthProperties(size_t const t_NumOfLayers,
                                                             size_t const t_Start,
                                                             size_t const t_End)
    {
        for(auto i = t_Start; i < t_End; ++i)
        {
            const auto curWL = m_CombinedLayerWavelengths[i];

            for(auto aSide : EnumSide())
            {
                for(size_t k = 0; k < t_NumOfLayers; ++k)
                {
                    m_TotA.at(aSide)->addProperties(
                      k, curWL, m_LayersWL[i].getLayerAbsorptances(k + 1, aSide));
                }
                for(auto aProperty : EnumPropertySimple())
                {
                    auto curPropertyMatrix = m_LayersWL[i].getProperty(aSide, aProperty);
                    m_Tot.at(std::make_pair(aSide, aProperty))
                      ->addProperties(curWL, curPropertyMatrix);
                }
            }
        }
    }

    void CEquivalentBSDFLayer::updateWavelengthLayers(CBSDFLayer & t_Layer)
    {
        const auto aResults = t_Layer.getWavelengthResults();
        const auto size = m_CombinedLayerWavelengths.size();
        for(size_t i = 0; i < size; ++i)
        {
            const auto curWL = m_CombinedLayerWavelengths[i];
            const auto index = t_Layer.getBandIndex(curWL);
            assert(index > -1);

            const std::shared_ptr<CBSDFIntegrator> currentLayer = (*aResults)[size_t(index)];

            if(m_LayersWL.size() <= i)
            {
                const CEquivalentBSDFLayerSingleBand aEquivalentLayer(currentLayer);

                m_LayersWL.push_back(aEquivalentLayer);
            }
            else
            {
                m_LayersWL[i].addLayer(currentLayer);
            }
        }
    }

}   // namespace MultiLayerOptics
