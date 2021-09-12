#include <cassert>

#include "MultiLayerInterRefSingleComponent.hpp"
#include "WCESingleLayerOptics.hpp"
#include "EquivalentLayerSingleComponent.hpp"

using namespace FenestrationCommon;
using namespace SingleLayerOptics;

namespace MultiLayerOptics
{
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////
    //   CSurfaceEnergy
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////
    CSurfaceEnergy::CSurfaceEnergy()
    {
        for(Side t_Side : EnumSide())
        {
            for(EnergyFlow t_EnergyFlow : EnumEnergyFlow())
            {
                m_IEnergy[std::make_pair(t_Side, t_EnergyFlow)] =
                  std::make_shared<std::vector<double>>();
            }
        }
    }

    void CSurfaceEnergy::addEnergy(const Side t_Side,
                                   const EnergyFlow t_EnergySide,
                                   const double t_Value)
    {
        m_IEnergy.at(std::make_pair(t_Side, t_EnergySide))->push_back(t_Value);
    }

    double
      CSurfaceEnergy::IEnergy(const size_t Index, const Side t_Side, const EnergyFlow t_EnergyFlow)
    {
        return (*m_IEnergy[std::make_pair(t_Side, t_EnergyFlow)])[Index - 1];
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////
    //   CInterRefSingleComponent
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////

    CInterRefSingleComponent::CInterRefSingleComponent(const double t_Tf,
                                                       const double t_Rf,
                                                       const double t_Tb,
                                                       const double t_Rb) :
        m_StateCalculated(false)
    {
        initialize(t_Tf, t_Rf, t_Tb, t_Rb);
    }

    CInterRefSingleComponent::CInterRefSingleComponent(const CLayerSingleComponent & t_Layer) :
        m_StateCalculated(false)
    {
        const double Tf = t_Layer.getProperty(Property::T, Side::Front);
        const double Rf = t_Layer.getProperty(Property::R, Side::Front);
        const double Tb = t_Layer.getProperty(Property::T, Side::Back);
        const double Rb = t_Layer.getProperty(Property::R, Side::Back);
        initialize(Tf, Rf, Tb, Rb);
    }

    void CInterRefSingleComponent::addLayer(
      const double t_Tf, const double t_Rf, const double t_Tb, const double t_Rb, const Side t_Side)
    {
        const CLayerSingleComponent aLayer(t_Tf, t_Rf, t_Tb, t_Rb);
        switch(t_Side)
        {
            case Side::Front:
                m_Layers.insert(m_Layers.begin(), aLayer);
                break;
            case Side::Back:
                m_Layers.push_back(aLayer);
                break;
            default:
                assert("Impossible side selection when adding new layer.");
                break;
        }
        m_StateCalculated = false;
    }

    void CInterRefSingleComponent::addLayer(const SingleLayerOptics::CLayerSingleComponent & tLayer,
                                            const Side t_Side)
    {
        const double Tf = tLayer.getProperty(Property::T, Side::Front);
        const double Rf = tLayer.getProperty(Property::R, Side::Front);
        const double Tb = tLayer.getProperty(Property::T, Side::Back);
        const double Rb = tLayer.getProperty(Property::R, Side::Back);
        addLayer(Tf, Rf, Tb, Rb, t_Side);
    }

    double CInterRefSingleComponent::getEnergyToSurface(const size_t Index,
                                                        const Side t_Side,
                                                        const EnergyFlow t_EnergyFlow)
    {
        calculateEnergies();
        return m_IEnergy.IEnergy(Index, t_Side, t_EnergyFlow);
    }

    const CSurfaceEnergy & CInterRefSingleComponent::getSurfaceEnergy()
    {
        calculateEnergies();
        return m_IEnergy;
    }

    double CInterRefSingleComponent::getLayerAbsorptance(const size_t Index, const Side t_Side)
    {
        // In this context side means energy flow, so we need to convert side into
        // correct energy flow
        const EnergyFlow aFlow = getFlowFromSide(t_Side);

        // Even if energy flow comes from one side, it still hits both sides of the layer and
        // this loop calculates energy absorbed at each side
        double absTot = 0;
        for(Side aSide : EnumSide())
        {
            absTot += m_Layers[Index - 1].getProperty(Property::Abs, aSide)
                      * getEnergyToSurface(Index, aSide, aFlow);
        }
        return absTot;
    }

    void CInterRefSingleComponent::initialize(const double t_Tf,
                                              const double t_Rf,
                                              const double t_Tb,
                                              const double t_Rb)
    {
        m_StateCalculated = false;
        addLayer(t_Tf, t_Rf, t_Tb, t_Rb);
    }

    void CInterRefSingleComponent::calculateEnergies()
    {
        if(!m_StateCalculated)
        {
            auto forwardLayers = calculateForwardLayers();
            auto backwardLayers = calculateBackwardLayers();

            for(size_t i = 0; i <= m_Layers.size(); ++i)
            {
                const CLayerSingleComponent & aForwardLayer = forwardLayers[i];
                const CLayerSingleComponent & aBackwardLayer = backwardLayers[i];

                const double Tf = aForwardLayer.getProperty(Property::T, Side::Front);
                const double Tb = aBackwardLayer.getProperty(Property::T, Side::Back);
                const double Rf = aBackwardLayer.getProperty(Property::R, Side::Front);
                const double Rb = aForwardLayer.getProperty(Property::R, Side::Back);
                const double iReflectance = 1 / (1 - Rf * Rb);

                if(i != m_Layers.size())
                {
                    m_IEnergy.addEnergy(Side::Front, EnergyFlow::Forward, Tf * iReflectance);
                    m_IEnergy.addEnergy(Side::Front, EnergyFlow::Backward, Tb * Rb * iReflectance);
                }

                if(i != 0)
                {
                    m_IEnergy.addEnergy(Side::Back, EnergyFlow::Forward, Tf * Rf * iReflectance);
                    m_IEnergy.addEnergy(Side::Back, EnergyFlow::Backward, Tb * iReflectance);
                }
            }

            m_StateCalculated = true;
        }
    }

    std::vector<SingleLayerOptics::CLayerSingleComponent>
      CInterRefSingleComponent::calculateForwardLayers()
    {
        std::vector<CLayerSingleComponent> forwardLayers;
        // Insert exterior environment properties
        CLayerSingleComponent aLayer(1, 0, 1, 0);
        forwardLayers.push_back(aLayer);

        // First layer just in. No calculation is needed
        aLayer = m_Layers[0];
        forwardLayers.push_back(aLayer);
        CEquivalentLayerSingleComponent aEqLayer(aLayer);
        for(size_t i = 1; i < m_Layers.size(); ++i)
        {
            aEqLayer.addLayer(m_Layers[i]);
            const auto layer = aEqLayer.getLayer();
            forwardLayers.push_back(layer);
        }
        return forwardLayers;
    }

    std::vector<SingleLayerOptics::CLayerSingleComponent>
      CInterRefSingleComponent::calculateBackwardLayers()
    {
        std::vector<CLayerSingleComponent> backwardLayers;
        // Insert interior environment properties
        CLayerSingleComponent aLayer(1, 0, 1, 0);
        backwardLayers.push_back(aLayer);

        const size_t size = m_Layers.size() - 1;
        // Last layer just in. No calculation is needed
        aLayer = m_Layers[size];
        backwardLayers.insert(backwardLayers.begin(), aLayer);
        CEquivalentLayerSingleComponent aEqLayer(aLayer);
        for(size_t i = size; i > 0; --i)
        {
            aEqLayer.addLayer(m_Layers[i - 1], Side::Front);
            const auto layer = aEqLayer.getLayer();
            backwardLayers.insert(backwardLayers.begin(), layer);
        }
        return backwardLayers;
    }

}   // namespace MultiLayerOptics
