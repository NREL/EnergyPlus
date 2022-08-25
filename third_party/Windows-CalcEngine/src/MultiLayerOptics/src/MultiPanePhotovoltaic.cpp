#include "MultiPanePhotovoltaic.hpp"

#include <cassert>

namespace MultiLayerOptics
{
    ///////////////////////////////////////////////////////////////////////////////////////
    // CMultiPanePhotovoltaic
    ///////////////////////////////////////////////////////////////////////////////////////

    std::unique_ptr<CMultiPanePhotovoltaic> CMultiPanePhotovoltaic::create(
      const std::vector<std::shared_ptr<SingleLayerOptics::SpecularLayer>> & layers,
      const FenestrationCommon::CSeries & t_SolarRadiation,
      const FenestrationCommon::CSeries & t_DetectorData)
    {
        return std::unique_ptr<CMultiPanePhotovoltaic>(
          new CMultiPanePhotovoltaic(layers, t_SolarRadiation, t_DetectorData));
    }

    CMultiPanePhotovoltaic::CMultiPanePhotovoltaic(
      const std::vector<std::shared_ptr<SingleLayerOptics::SpecularLayer>> & layers,
      const FenestrationCommon::CSeries & t_SolarRadiation,
      const FenestrationCommon::CSeries & t_DetectorData) :
        CMultiPaneSpecular(layers, t_SolarRadiation, t_DetectorData)
    {}

    CMultiPanePhotovoltaic::CMultiPanePhotovoltaic(
      std::vector<double> const & t_CommonWavelength,
      const FenestrationCommon::CSeries & t_SolarRadiation,
      const std::shared_ptr<SingleLayerOptics::SpecularLayer> & t_Layer) :
        CMultiPaneSpecular(t_CommonWavelength, t_SolarRadiation, t_Layer)
    {}

    double
      CMultiPanePhotovoltaic::AbsHeat(const size_t Index,
                                      const double t_Angle,
                                      const double minLambda,
                                      const double maxLambda,
                                      const FenestrationCommon::IntegrationType t_IntegrationType,
                                      const double normalizationCoefficient)
    {
        return Abs(
                 Index, t_Angle, minLambda, maxLambda, t_IntegrationType, normalizationCoefficient)
               - AbsElectricity(
                 Index, t_Angle, minLambda, maxLambda, t_IntegrationType, normalizationCoefficient);
    }

    double
      CMultiPanePhotovoltaic::AbsElectricity(size_t Index,
                                             double t_Angle,
                                             double minLambda,
                                             double maxLambda,
                                             FenestrationCommon::IntegrationType t_IntegrationType,
                                             double normalizationCoefficient)
    {
        if(dynamic_cast<SingleLayerOptics::PhotovoltaicLayer *>(m_Layers[Index - 1].get())
           != nullptr)
        {
            const double totalSolar =
              m_SolarRadiation.integrate(t_IntegrationType, normalizationCoefficient)
                ->sum(minLambda, maxLambda);

            CEquivalentLayerSingleComponentMWAngle aAngularProperties = getAngular(t_Angle);
            auto aLayer =
              dynamic_cast<SingleLayerOptics::PhotovoltaicLayer *>(m_Layers[Index - 1].get());

            auto frontJscPrime = aLayer->jscPrime(FenestrationCommon::Side::Front);
            // auto backJscPrime = aLayer->jscPrime(FenestrationCommon::Side::Back);

            // auto IPlus = aAngularProperties.iplus(Index - 1);
            auto IMinus = aAngularProperties.iminus(Index - 1);

            auto frontJsc = frontJscPrime * IMinus;
            auto JscIntegrated = frontJsc.integrate(t_IntegrationType, normalizationCoefficient);
            auto jsc{JscIntegrated->sum() * totalSolar};

            const auto voc{aLayer->voc(jsc)};
            const auto ff{aLayer->ff(jsc)};

            const auto power{jsc * voc * ff};

            assert(totalSolar > 0);
            return power / totalSolar;
        }

        return 0;
    }
}   // namespace MultiLayerOptics