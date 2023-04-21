#include <cassert>
#include <algorithm>
#include <cmath>
#include <utility>

#include "MultiPaneSpecular.hpp"
#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"

using namespace FenestrationCommon;
using namespace SingleLayerOptics;

namespace MultiLayerOptics
{
    ////////////////////////////////////////////////////////////////////////////////////////////
    //  CEquivalentLayerSingleComponentMWAngle
    ////////////////////////////////////////////////////////////////////////////////////////////
    CEquivalentLayerSingleComponentMWAngle::CEquivalentLayerSingleComponentMWAngle(
      CEquivalentLayerSingleComponentMW t_Layer, CAbsorptancesMultiPane t_Abs, double t_Angle) :
        m_Layer(std::move(t_Layer)),
        m_Abs(std::move(t_Abs)),
        m_Angle(t_Angle)
    {}

    double CEquivalentLayerSingleComponentMWAngle::angle() const
    {
        return m_Angle;
    }

    const CEquivalentLayerSingleComponentMW & CEquivalentLayerSingleComponentMWAngle::layer() const
    {
        return m_Layer;
    }

    CSeries CEquivalentLayerSingleComponentMWAngle::getProperties(const Side t_Side,
                                                                  const Property t_Property)
    {
        return m_Layer.getProperties(t_Property, t_Side);
    }

    CSeries CEquivalentLayerSingleComponentMWAngle::Abs(size_t const Index)
    {
        return m_Abs.Abs(Index);
    }

    CSeries CEquivalentLayerSingleComponentMWAngle::iplus(size_t Index)
    {
        return m_Abs.iplus(Index);
    }

    CSeries CEquivalentLayerSingleComponentMWAngle::iminus(size_t Index)
    {
        return m_Abs.iminus(Index);
    }

    ////////////////////////////////////////////////////////////////////////////////////////////
    //  CMultiPaneSpecular
    ////////////////////////////////////////////////////////////////////////////////////////////
    CMultiPaneSpecular::CMultiPaneSpecular(
      const std::vector<std::shared_ptr<SingleLayerOptics::SpecularLayer>> & layers,
      const CSeries & t_SolarRadiation,
      const CSeries & t_DetectorData) :
        m_Layers(layers),
        m_SolarRadiation(t_SolarRadiation),
        m_DetectorData(t_DetectorData)
    {
        CCommonWavelengths aCommonWL;
        for(auto & layer : m_Layers)
        {
            aCommonWL.addWavelength(layer->getBandWavelengths());
        }

        // Finds combination of two wavelength sets without going outside of wavelenght range for
        // any of spectral samples.
        m_CommonWavelengths = aCommonWL.getCombinedWavelengths(Combine::Interpolate);

        m_SolarRadiation = m_SolarRadiation.interpolate(m_CommonWavelengths);

        if(m_DetectorData.size() > 0)
        {
            m_DetectorData.interpolate(m_CommonWavelengths);
        }

        for(auto & layer : m_Layers)
        {
            layer->setSourceData(m_SolarRadiation);
        }
    }

    CMultiPaneSpecular::CMultiPaneSpecular(const std::vector<double> & t_CommonWavelength,
                                           const CSeries & t_SolarRadiation,
                                           const std::shared_ptr<SpecularLayer> & t_Layer) :
        m_CommonWavelengths(t_CommonWavelength),
        m_SolarRadiation(t_SolarRadiation)
    {
        m_SolarRadiation = m_SolarRadiation.interpolate(m_CommonWavelengths);
        addLayer(t_Layer);
    }

    void CMultiPaneSpecular::addLayer(
      const std::shared_ptr<SingleLayerOptics::SpecularLayer> & t_Layer)
    {
        t_Layer->setSourceData(m_SolarRadiation);
        m_Layers.push_back(t_Layer);
    }

    std::unique_ptr<CMultiPaneSpecular> CMultiPaneSpecular::create(
      const std::vector<std::shared_ptr<SingleLayerOptics::SpecularLayer>> & layers,
      const CSeries & t_SolarRadiation,
      const CSeries & t_DetectorData)
    {
        return std::unique_ptr<CMultiPaneSpecular>(
          new CMultiPaneSpecular(layers, t_SolarRadiation, t_DetectorData));
    }

    double CMultiPaneSpecular::getPropertySimple(
      PropertySimple t_Property, Side t_Side, Scattering t_Scattering, double t_Theta, double)
    {
        return getPropertySimple(
          getMinLambda(), getMaxLambda(), t_Property, t_Side, t_Scattering, t_Theta);
    }

    double CMultiPaneSpecular::getPropertySimple(const double minLambda,
                                                 const double maxLambda,
                                                 FenestrationCommon::PropertySimple t_Property,
                                                 FenestrationCommon::Side t_Side,
                                                 FenestrationCommon::Scattering t_Scattering,
                                                 double t_Theta,
                                                 double)
    {
        double result(0);
        const auto prop(toProperty(t_Property));
        switch(t_Scattering)
        {
            case Scattering::DirectDirect:
                result = getProperty(t_Side, prop, t_Theta, minLambda, maxLambda);
                break;
            case Scattering::DiffuseDiffuse:
                result = getHemisphericalProperty(
                  t_Side, prop, {0, 10, 20, 30, 40, 50, 60, 70, 80, 90}, minLambda, maxLambda);
                break;
            case Scattering::DirectDiffuse:
                result = 0;
                break;
            case Scattering::DirectHemispherical:
                result = getProperty(t_Side, prop, t_Theta, minLambda, maxLambda);
                break;
        }

        return result;
    }

    double CMultiPaneSpecular::getMinLambda() const
    {
        return m_Layers[0]->getMinLambda();
    }

    double CMultiPaneSpecular::getMaxLambda() const
    {
        return m_Layers[0]->getMaxLambda();
    }

    std::vector<double> CMultiPaneSpecular::getWavelengths() const
    {
        return m_CommonWavelengths;
    }

    double CMultiPaneSpecular::getProperty(const Side t_Side,
                                           const Property t_Property,
                                           const double t_Angle,
                                           const double minLambda,
                                           const double maxLambda,
                                           const IntegrationType t_IntegrationType,
                                           double normalizationCoefficient)
    {
        CEquivalentLayerSingleComponentMWAngle aAngularProperties = getAngular(t_Angle);

        auto aProperties = aAngularProperties.getProperties(t_Side, t_Property);

        auto solarRadiation = m_SolarRadiation;

        if(m_DetectorData.size() > 0)
        {
            if(m_DetectorData.size() != solarRadiation.size())
            {
                m_DetectorData = m_DetectorData.interpolate(solarRadiation.getXArray());    
            }
            solarRadiation = solarRadiation * m_DetectorData;
        }

        auto aMult = aProperties * solarRadiation;

        const auto iIntegrated = aMult.integrate(t_IntegrationType, normalizationCoefficient);

        // TODO: Check detector data here and multiply with it if necessary

        const double totalProperty = iIntegrated->sum(minLambda, maxLambda);

        double totalSolar = solarRadiation.integrate(t_IntegrationType, normalizationCoefficient)
                              ->sum(minLambda, maxLambda);

        assert(totalSolar > 0);

        return totalProperty / totalSolar;
    }

    double
      CMultiPaneSpecular::getHemisphericalProperty(Side t_Side,
                                                   Property t_Property,
                                                   const std::vector<double> & t_IntegrationAngles,
                                                   double minLambda,
                                                   double maxLambda,
                                                   IntegrationType t_IntegrationType,
                                                   double normalizationCoefficient)
    {
        size_t size = t_IntegrationAngles.size();
        std::shared_ptr<CSeries> aAngularProperties = std::make_shared<CSeries>();
        for(size_t i = 0; i < size; ++i)
        {
            double angle = t_IntegrationAngles[i];
            double aProperty = getProperty(t_Side,
                                           t_Property,
                                           angle,
                                           minLambda,
                                           maxLambda,
                                           t_IntegrationType,
                                           normalizationCoefficient);
            aAngularProperties->addProperty(angle, aProperty);
        }
        CHemispherical2DIntegrator aIntegrator = CHemispherical2DIntegrator(
          *aAngularProperties, t_IntegrationType, normalizationCoefficient);
        return aIntegrator.value();
    }

    double CMultiPaneSpecular::getAbsorptanceLayer(size_t index,
                                                   FenestrationCommon::Side side,
                                                   FenestrationCommon::ScatteringSimple scattering,
                                                   double theta,
                                                   double)
    {
        return getAbsorptanceLayer(getMinLambda(), getMaxLambda(), index, side, scattering, theta);
    }

    double CMultiPaneSpecular::getAbsorptanceLayer(double minLambda,
                                                   double maxLambda,
                                                   size_t index,
                                                   FenestrationCommon::Side,
                                                   FenestrationCommon::ScatteringSimple scattering,
                                                   double theta,
                                                   double)
    {
        auto result(0.0);
        if(scattering == ScatteringSimple::Direct)
        {
            result = Abs(index, theta, minLambda, maxLambda);
        }
        else if(scattering == ScatteringSimple::Diffuse)
        {
            result = AbsHemispherical(
              index, {0, 10, 20, 30, 40, 50, 60, 70, 80, 90}, minLambda, maxLambda);
        }
        return result;
    }

    std::vector<double>
      CMultiPaneSpecular::getAbsorptanceLayers(double minLambda,
                                               double maxLambda,
                                               FenestrationCommon::Side side,
                                               FenestrationCommon::ScatteringSimple scattering,
                                               double theta,
                                               double phi)
    {
        std::vector<double> res;
        for(size_t i = 1u; i <= size(); ++i)
        {
            res.push_back(
              getAbsorptanceLayer(minLambda, maxLambda, i, side, scattering, theta, phi));
        }

        return res;
    }

    double CMultiPaneSpecular::Abs(size_t const Index,
                                   const double t_Angle,
                                   const double minLambda,
                                   const double maxLambda,
                                   const IntegrationType t_IntegrationType,
                                   double normalizationCoefficient)
    {
        CEquivalentLayerSingleComponentMWAngle aAngularProperties = getAngular(t_Angle);
        auto aProperties = aAngularProperties.Abs(Index - 1);

        auto aMult = aProperties * m_SolarRadiation;

        auto iIntegrated = aMult.integrate(t_IntegrationType, normalizationCoefficient);

        double totalProperty = iIntegrated->sum(minLambda, maxLambda);
        double totalSolar = m_SolarRadiation.integrate(t_IntegrationType, normalizationCoefficient)
                              ->sum(minLambda, maxLambda);

        assert(totalSolar > 0);

        return totalProperty / totalSolar;
    }

    std::vector<double>
      CMultiPaneSpecular::Absorptances(double t_Angle,
                                       double minLambda,
                                       double maxLambda,
                                       FenestrationCommon::IntegrationType t_IntegrationType,
                                       double normalizationCoefficient)
    {
        std::vector<double> res;
        for(size_t i = 1u; i <= size(); ++i)
        {
            res.push_back(
              Abs(i, t_Angle, minLambda, maxLambda, t_IntegrationType, normalizationCoefficient));
        }

        return res;
    }

    double CMultiPaneSpecular::AbsHemispherical(size_t const Index,
                                                const std::vector<double> & t_IntegrationAngles,
                                                const double minLambda,
                                                const double maxLambda,
                                                const IntegrationType t_IntegrationType,
                                                double normalizationCoefficient)
    {
        size_t size = t_IntegrationAngles.size();
        std::shared_ptr<CSeries> aAngularProperties = std::make_shared<CSeries>();
        for(size_t i = 0; i < size; ++i)
        {
            double angle = t_IntegrationAngles[i];
            double aAbs = Abs(Index, angle, minLambda, maxLambda, t_IntegrationType);
            aAngularProperties->addProperty(angle, aAbs);
        }

        CHemispherical2DIntegrator aIntegrator = CHemispherical2DIntegrator(
          *aAngularProperties, t_IntegrationType, normalizationCoefficient);
        return aIntegrator.value();
    }

    CEquivalentLayerSingleComponentMWAngle CMultiPaneSpecular::getAngular(const double t_Angle)
    {
        std::vector<CEquivalentLayerSingleComponentMWAngle>::iterator it;
        it = find_if(m_EquivalentAngle.begin(),
                     m_EquivalentAngle.end(),
                     [&t_Angle](const CEquivalentLayerSingleComponentMWAngle & obj) {
                         return std::abs(obj.angle() - t_Angle) < 1e-6;
                     });

        return (it != m_EquivalentAngle.end()) ? (*it) : createNewAngular(t_Angle);
    }

    CEquivalentLayerSingleComponentMWAngle
      CMultiPaneSpecular::createNewAngular(const double t_Angle)
    {
        // Create direction for specular. It is irrelevant what is Phi angle and it is chosen to be
        // zero in this case
        CBeamDirection aDirection = CBeamDirection(t_Angle, 0);
        auto firstLayerResults = getSeriesResults(aDirection, 0);
        CEquivalentLayerSingleComponentMW aEqLayer{
          firstLayerResults.T, firstLayerResults.T, firstLayerResults.Rf, firstLayerResults.Rb};
        CAbsorptancesMultiPane aAbs{
          firstLayerResults.T, firstLayerResults.Rf, firstLayerResults.Rb};
        for(size_t i = 1u; i < m_Layers.size(); ++i)
        {
            auto layRes = getSeriesResults(aDirection, i);
            aEqLayer.addLayer(layRes.T, layRes.T, layRes.Rf, layRes.Rb);
            aAbs.addLayer(layRes.T, layRes.Rf, layRes.Rb);
        }

        CEquivalentLayerSingleComponentMWAngle newLayer(aEqLayer, aAbs, t_Angle);

        m_EquivalentAngle.push_back(newLayer);

        return newLayer;
    }

    CMultiPaneSpecular::SeriesResults
      CMultiPaneSpecular::getSeriesResults(const CBeamDirection & aDirection, size_t layerIndex)
    {
        SeriesResults result;

        std::vector<double> wl = m_Layers[layerIndex]->getBandWavelengths();
        std::vector<double> Tv = m_Layers[layerIndex]->T_dir_dir_band(Side::Front, aDirection);
        std::vector<double> Rfv = m_Layers[layerIndex]->R_dir_dir_band(Side::Front, aDirection);
        std::vector<double> Rbv = m_Layers[layerIndex]->R_dir_dir_band(Side::Back, aDirection);
        for(size_t j = 0; j < wl.size(); ++j)
        {
            auto tr = checkRange(Tv[j], Rfv[j]);
            Tv[j] = tr.T;
            Rfv[j] = tr.R;

            tr = checkRange(Tv[j], Rbv[j]);
            Tv[j] = tr.T;
            Rbv[j] = tr.R;
            result.T.addProperty(wl[j], Tv[j]);
            result.Rf.addProperty(wl[j], Rfv[j]);
            result.Rb.addProperty(wl[j], Rbv[j]);
        }

        result.T = result.T.interpolate(m_CommonWavelengths);
        result.Rf = result.Rf.interpolate(m_CommonWavelengths);
        result.Rb = result.Rb.interpolate(m_CommonWavelengths);

        return result;
    }

    size_t CMultiPaneSpecular::size() const
    {
        return m_Layers.size();
    }


}   // namespace MultiLayerOptics
