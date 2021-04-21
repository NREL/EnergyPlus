#include <cassert>
#include <stdexcept>

#include "MultiLayerInterRef.hpp"
#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"
#include "EquivalentScatteringLayer.hpp"
#include "MultiLayerInterRefSingleComponent.hpp"

using namespace FenestrationCommon;
using namespace SingleLayerOptics;

namespace MultiLayerOptics
{
    CInterRef::CInterRef(CScatteringLayer & t_Layer, const double t_Theta, const double t_Phi) :
        m_StackedLayers({{Side::Front, CLayer_List()}, {Side::Back, CLayer_List()}}),
        m_DirectComponent(t_Layer.getLayer(Scattering::DirectDirect, t_Theta, t_Phi)),
        m_DiffuseComponent(t_Layer.getLayer(Scattering::DiffuseDiffuse, t_Theta, t_Phi)),
        m_Energy({{Scattering::DirectDirect, CSurfaceEnergy()},
                  {Scattering::DirectDiffuse, CSurfaceEnergy()},
                  {Scattering::DiffuseDiffuse, CSurfaceEnergy()}}),
        m_Abs({{std::make_pair(Side::Front, ScatteringSimple::Diffuse), std::vector<double>()},
               {std::make_pair(Side::Back, ScatteringSimple::Diffuse), std::vector<double>()},
               {std::make_pair(Side::Front, ScatteringSimple::Direct), std::vector<double>()},
               {std::make_pair(Side::Back, ScatteringSimple::Direct), std::vector<double>()}}),
        m_StateCalculated(false),
        m_Theta(t_Theta),
        m_Phi(t_Phi)
    {
        m_Layers.push_back(t_Layer);
    }

    void CInterRef::addLayer(CScatteringLayer & t_Layer,
                             const Side t_Side,
                             const double t_Theta,
                             const double t_Phi)
    {
        switch(t_Side)
        {
            case Side::Front:
                m_Layers.insert(m_Layers.begin(), t_Layer);
                break;
            case Side::Back:
                m_Layers.push_back(t_Layer);
                break;
            default:
                assert("Impossible side selection when adding new layer.");
                break;
        }

        // addition for pure components (direct and diffuse)
        m_DirectComponent.addLayer(t_Layer.getLayer(Scattering::DirectDirect, t_Theta, t_Phi),
                                   t_Side);
        m_DiffuseComponent.addLayer(t_Layer.getLayer(Scattering::DiffuseDiffuse, t_Theta, t_Phi),
                                    t_Side);

        m_StateCalculated = false;
    }

    double CInterRef::getAbsorptance(const size_t Index,
                                     Side t_Side,
                                     ScatteringSimple t_Scattering,
                                     const double t_Theta,
                                     const double t_Phi)
    {
        calculateEnergies(t_Theta, t_Phi);
        std::vector<double> & aVector = m_Abs.at(std::make_pair(t_Side, t_Scattering));
        size_t vecSize = aVector.size();
        if(vecSize < Index)
        {
            throw std::range_error("Requested layer index is out of range.");
        }
        return aVector[Index - 1];
    }

    double CInterRef::getEnergyToSurface(const size_t Index,
                                         const Side t_SurfaceSide,
                                         const EnergyFlow t_EnergyFlow,
                                         const Scattering t_Scattering,
                                         const double t_Theta,
                                         const double t_Phi)
    {
        calculateEnergies(t_Theta, t_Phi);
        return m_Energy.at(t_Scattering).IEnergy(Index, t_SurfaceSide, t_EnergyFlow);
    }

    size_t CInterRef::size() const
    {
        return m_Layers.size();
    }

    void CInterRef::calculateEnergies(const double t_Theta, const double t_Phi)
    {
        if((!m_StateCalculated) || (t_Theta != m_Theta) || (t_Phi != m_Phi))
        {
            createForwardLayers(t_Theta, t_Phi);
            createBackwardLayers(t_Theta, t_Phi);

            m_Energy[Scattering::DirectDirect] = m_DirectComponent.getSurfaceEnergy();
            m_Energy[Scattering::DiffuseDiffuse] = m_DiffuseComponent.getSurfaceEnergy();
            m_Energy[Scattering::DirectDiffuse] = calcDirectToDiffuseComponent(t_Theta, t_Phi);

            calculateAbsroptances(t_Theta, t_Phi);

            m_StateCalculated = true;
            m_Theta = t_Theta;
            m_Phi = t_Phi;
        }
    }

    void CInterRef::createForwardLayers(const double t_Theta, const double t_Phi)
    {
        CLayer_List & aLayers = m_StackedLayers.at(Side::Front);

        // Insert exterior environment first
        const CScatteringSurface aFront(1, 0, 0, 0, 1, 0);
        const CScatteringSurface aBack(1, 0, 0, 0, 1, 0);
        const CScatteringLayer exterior(aFront, aBack);
        aLayers.push_back(exterior);

        auto & aLayer = m_Layers[0];
        aLayers.push_back(aLayer);
        CEquivalentScatteringLayer aEqLayer(aLayer, t_Theta, t_Phi);
        for(size_t i = 1; i < m_Layers.size(); ++i)
        {
            aEqLayer.addLayer(m_Layers[i], Side::Back, t_Theta, t_Phi);
            aLayers.push_back(aEqLayer.getLayer());
        }

        aLayers.push_back(exterior);
    }

    void CInterRef::createBackwardLayers(const double t_Theta, const double t_Phi)
    {
        CLayer_List & aLayers = m_StackedLayers.at(Side::Back);

        // Insert interior environment
        const CScatteringSurface aFront(1, 0, 0, 0, 1, 0);
        const CScatteringSurface aBack(1, 0, 0, 0, 1, 0);
        const CScatteringLayer exterior(aFront, aBack);
        aLayers.push_back(exterior);

        const size_t size = m_Layers.size() - 1;
        // Last layer just in
        auto & aLayer = m_Layers[size];
        aLayers.insert(aLayers.begin(), aLayer);
        CEquivalentScatteringLayer aEqLayer = CEquivalentScatteringLayer(aLayer, t_Theta, t_Phi);
        for(size_t i = size; i > 0; --i)
        {
            aEqLayer.addLayer(m_Layers[i - 1], Side::Front, t_Theta, t_Phi);
            aLayers.insert(aLayers.begin(), aEqLayer.getLayer());
        }
        aLayers.insert(aLayers.begin(), exterior);
    }

    CSurfaceEnergy CInterRef::calcDiffuseEnergy(const double t_Theta, const double t_Phi)
    {
        // Sum of previous two components. Total diffuse energy that gets off the surfaces.
        CSurfaceEnergy diffSum{};

        for(EnergyFlow aEnergyFlow : EnumEnergyFlow())
        {
            for(size_t i = 1; i <= m_Layers.size(); ++i)
            {   // Layer indexing goes from one
                for(Side aSide : EnumSide())
                {
                    Side oppSide = oppositeSide(aSide);
                    // Calculate diffuse energy from direct exterior/interior beam
                    double beamEnergy = 0;

                    auto & curLayer = m_StackedLayers.at(oppSide)[i];

                    if((aSide == Side::Front && aEnergyFlow == EnergyFlow::Backward)
                       || (aSide == Side::Back && aEnergyFlow == EnergyFlow::Forward))
                    {
                        beamEnergy = curLayer.getPropertySimple(curLayer.getMinLambda(),
                                                                curLayer.getMaxLambda(),
                                                                PropertySimple::T,
                                                                oppSide,
                                                                Scattering::DirectDiffuse,
                                                                t_Theta,
                                                                t_Phi);
                    }

                    // Energy that gets converted to diffuse from beam that comes from
                    // interreflections in the gap or interior/exterior environments
                    double R = curLayer.getPropertySimple(curLayer.getMinLambda(),
                                                          curLayer.getMaxLambda(),
                                                          PropertySimple::R,
                                                          aSide,
                                                          Scattering::DirectDiffuse,
                                                          t_Theta,
                                                          t_Phi);
                    const double intEnergy =
                      R * m_Energy.at(Scattering::DirectDirect).IEnergy(i, aSide, aEnergyFlow);
                    diffSum.addEnergy(aSide, aEnergyFlow, beamEnergy + intEnergy);
                }
            }
        }

        return diffSum;
    }

    CSurfaceEnergy CInterRef::calcDirectToDiffuseComponent(const double t_Theta, const double t_Phi)
    {
        // Gets total diffuse components that is getting off (leaving) every surface.
        // Keep in mind that diffuse componet here only comes from scattering direct beam.
        CSurfaceEnergy diffSum{calcDiffuseEnergy(t_Theta, t_Phi)};

        // Now need to calculate interreflections of total diffuse components that are leaving
        // every surface and calculate total diffuse component that is incoming to every surface.
        CSurfaceEnergy aScatter{};

        // Calculate total energy scatterred from beam to diffuse
        for(EnergyFlow aEnergyFlow : EnumEnergyFlow())
        {
            // In this case numbering goes through gas environments (gaps, interior and exterior)
            // becase we want to keep interreflectance calculations together
            for(size_t i = 0; i <= m_Layers.size(); ++i)
            {
                auto & fwdLayer = m_StackedLayers.at(Side::Front)[i];
                auto & bkwLayer = m_StackedLayers.at(Side::Back)[i + 1];
                double Ib = 0;
                if(i != 0)
                {
                    Ib = diffSum.IEnergy(i, Side::Back, aEnergyFlow);
                }
                double If = 0;
                if(i != m_Layers.size())
                {
                    If = diffSum.IEnergy(i + 1, Side::Front, aEnergyFlow);
                }
                const double Rf_bkw = bkwLayer.getPropertySimple(bkwLayer.getMinLambda(), bkwLayer.getMaxLambda(),
                  PropertySimple::R, Side::Front, Scattering::DiffuseDiffuse, t_Theta, t_Phi);
                const double Rb_fwd = fwdLayer.getPropertySimple(fwdLayer.getMinLambda(), fwdLayer.getMaxLambda(),
                  PropertySimple::R, Side::Back, Scattering::DiffuseDiffuse, t_Theta, t_Phi);
                const double interRef = 1 / (1 - Rf_bkw * Rb_fwd);
                const double Ib_tot = (Ib * Rf_bkw + If) * interRef;
                const double If_tot = (Ib + Rb_fwd * If) * interRef;
                if(i != 0)
                {
                    aScatter.addEnergy(Side::Back, aEnergyFlow, Ib_tot);
                }
                if(i != m_Layers.size())
                {
                    aScatter.addEnergy(Side::Front, aEnergyFlow, If_tot);
                }
            }
        }

        return aScatter;
    }

    void CInterRef::calculateAbsroptances(const double t_Theta, const double t_Phi)
    {
        for(size_t i = 0; i < m_Layers.size(); ++i)
        {
            for(EnergyFlow aEnergyFlow : EnumEnergyFlow())
            {
                double EnergyDirect = 0;
                double EnergyDiffuse = 0;
                for(Side aSide : EnumSide())
                {
                    const double Adir =
                      m_Layers[i].getAbsorptance(aSide, ScatteringSimple::Direct, t_Theta, t_Phi);
                    EnergyDirect +=
                      Adir * m_Energy[Scattering::DirectDirect].IEnergy(i + 1, aSide, aEnergyFlow);
                    const double Adif =
                      m_Layers[i].getAbsorptance(aSide, ScatteringSimple::Diffuse, t_Theta, t_Phi);
                    EnergyDirect +=
                      Adif * m_Energy[Scattering::DirectDiffuse].IEnergy(i + 1, aSide, aEnergyFlow);
                    EnergyDiffuse +=
                      Adif
                      * m_Energy[Scattering::DiffuseDiffuse].IEnergy(i + 1, aSide, aEnergyFlow);
                }
                // Note that front and back absorptances are actually reffereing to forward and
                // backward energy flows. That is why we need this conversion.
                Side flowSide(getSideFromFlow(aEnergyFlow));
                m_Abs.at(std::make_pair(flowSide, ScatteringSimple::Direct))
                  .push_back(EnergyDirect);
                m_Abs.at(std::make_pair(flowSide, ScatteringSimple::Diffuse))
                  .push_back(EnergyDiffuse);
            }
        }
    }

}   // namespace MultiLayerOptics
