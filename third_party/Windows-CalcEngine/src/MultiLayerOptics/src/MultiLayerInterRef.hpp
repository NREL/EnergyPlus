#ifndef MULTILAYERINTERREF_H
#define MULTILAYERINTERREF_H

#include <memory>
#include <vector>
#include <map>

#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"
#include "MultiLayerInterRefSingleComponent.hpp"

namespace FenestrationCommon
{
    enum class Scattering;
}

namespace SingleLayerOptics
{
    class CScatteringLayer;
}

namespace MultiLayerOptics
{
    typedef std::vector<SingleLayerOptics::CScatteringLayer> CLayer_List;

    class CInterRef
    {
    public:
        CInterRef(SingleLayerOptics::CScatteringLayer & t_Layer,
                  const double t_Theta = 0,
                  const double t_Phi = 0);

        void addLayer(SingleLayerOptics::CScatteringLayer & t_Layer,
                      const FenestrationCommon::Side t_Side = FenestrationCommon::Side::Back,
                      const double t_Theta = 0,
                      const double t_Phi = 0);

        double getAbsorptance(const size_t Index,
                              FenestrationCommon::Side t_Side,
                              FenestrationCommon::ScatteringSimple t_Scattering,
                              const double t_Theta = 0,
                              const double t_Phi = 0);

        double getEnergyToSurface(const size_t Index,
                                  const FenestrationCommon::Side t_SurfaceSide,
                                  const FenestrationCommon::EnergyFlow t_EnergyFlow,
                                  const FenestrationCommon::Scattering t_Scattering,
                                  const double t_Theta = 0,
                                  const double t_Phi = 0);

        size_t size() const;

    private:
        void calculateEnergies(const double t_Theta, const double t_Phi);
        void createForwardLayers(const double t_Theta, const double t_Phi);
        void createBackwardLayers(const double t_Theta, const double t_Phi);

        // Function that calculate total diffuse energy that is leaving surface
        // and that originates from direct beam
        CSurfaceEnergy calcDiffuseEnergy(const double t_Theta, const double t_Phi);

        // Calculate direct to diffuse component at each surface
        CSurfaceEnergy calcDirectToDiffuseComponent(const double t_Theta, const double t_Phi);

        void calculateAbsroptances(const double t_Theta, const double t_Phi);

        std::vector<SingleLayerOptics::CScatteringLayer> m_Layers;

        std::map<FenestrationCommon::Side, CLayer_List> m_StackedLayers;

        // for calculation of pure components (direct and diffuse)
        CInterRefSingleComponent m_DirectComponent;
        CInterRefSingleComponent m_DiffuseComponent;

        // Energy that is incoming at each surface. It contains three different components:
        // 1. Direct beam energy component calculates how much of direct beam will be incoming at
        //   each surface.
        // 2. Diffuse component that originates from incoming direct beam.
        // 3. Diffuse component that originates from incoming diffuse.
        std::map<FenestrationCommon::Scattering, CSurfaceEnergy> m_Energy;

        // Absorptance for each layer comes in two different forms: Absrobed from diffuse and
        // absorbled from direct.
        std::map<std::pair<FenestrationCommon::Side, FenestrationCommon::ScatteringSimple>,
                 std::vector<double>>
          m_Abs;

        bool m_StateCalculated;
        double m_Theta;
        double m_Phi;
    };

}   // namespace MultiLayerOptics

#endif
