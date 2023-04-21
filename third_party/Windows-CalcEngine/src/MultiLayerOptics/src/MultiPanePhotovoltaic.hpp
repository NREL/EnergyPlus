#pragma once

#include <vector>
#include <WCECommon.hpp>
#include <WCESingleLayerOptics.hpp>
#include "MultiPaneSpecular.hpp"

namespace MultiLayerOptics
{
    ///////////////////////////////////////////////////////////////////////////////////////
    // CMultiPanePhotovoltaic
    ///////////////////////////////////////////////////////////////////////////////////////

    class CMultiPanePhotovoltaic : public CMultiPaneSpecular
    {
    public:
        static std::unique_ptr<CMultiPanePhotovoltaic> create(
          const std::vector<std::shared_ptr<SingleLayerOptics::SpecularLayer>> & layers,
          const FenestrationCommon::CSeries & t_SolarRadiation,
          const FenestrationCommon::CSeries & t_DetectorData = FenestrationCommon::CSeries());

        CMultiPanePhotovoltaic(
          const std::vector<std::shared_ptr<SingleLayerOptics::SpecularLayer>> & layers,
          const FenestrationCommon::CSeries & t_SolarRadiation,
          const FenestrationCommon::CSeries & t_DetectorData = FenestrationCommon::CSeries());

        CMultiPanePhotovoltaic(const std::vector<double> & t_CommonWavelength,
                               const FenestrationCommon::CSeries & t_SolarRadiation,
                               const std::shared_ptr<SingleLayerOptics::SpecularLayer> & t_Layer);

        double AbsHeat(size_t Index,
                       double t_Angle,
                       double minLambda,
                       double maxLambda,
                       FenestrationCommon::IntegrationType t_IntegrationType =
                         FenestrationCommon::IntegrationType::Trapezoidal,
                       double normalizationCoefficient = 1);

        double AbsElectricity(size_t Index,
                              double t_Angle,
                              double minLambda,
                              double maxLambda,
                              FenestrationCommon::IntegrationType t_IntegrationType =
                                FenestrationCommon::IntegrationType::Trapezoidal,
                              double normalizationCoefficient = 1);
    };
}   // namespace MultiLayerOptics