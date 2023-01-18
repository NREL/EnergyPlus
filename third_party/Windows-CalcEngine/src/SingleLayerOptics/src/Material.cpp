#include <stdexcept>

#include "Material.hpp"
#include "WCECommon.hpp"
#include "MaterialDescription.hpp"
#include "WCESpectralAveraging.hpp"

using FenestrationCommon::WavelengthRange;
using FenestrationCommon::CWavelengthRange;
using FenestrationCommon::CSeries;
using SpectralAveraging::CSpectralSample;
using SpectralAveraging::CPhotovoltaicSample;

namespace SingleLayerOptics
{
    std::shared_ptr<CMaterial> Material::dualBandMaterial(const double Tfsol,
                                                          const double Tbsol,
                                                          const double Rfsol,
                                                          const double Rbsol,
                                                          const double Tfvis,
                                                          const double Tbvis,
                                                          const double Rfvis,
                                                          const double Rbvis)
    {
        auto aSolarRangeMaterial =
          std::make_shared<CMaterialSingleBand>(Tfsol, Tbsol, Rfsol, Rbsol, WavelengthRange::Solar);
        auto aVisibleRangeMaterial = std::make_shared<CMaterialSingleBand>(
          Tfvis, Tbvis, Rfvis, Rbvis, WavelengthRange::Visible);
        return std::make_shared<CMaterialDualBand>(aVisibleRangeMaterial, aSolarRangeMaterial);
    }

    std::shared_ptr<CMaterial> Material::dualBandMaterial(const double Tfsol,
                                                          const double Tbsol,
                                                          const double Rfsol,
                                                          const double Rbsol,
                                                          const double Tfvis,
                                                          const double Tbvis,
                                                          const double Rfvis,
                                                          const double Rbvis,
                                                          const double ratio)
    {
        auto aSolarRangeMaterial =
          std::make_shared<CMaterialSingleBand>(Tfsol, Tbsol, Rfsol, Rbsol, WavelengthRange::Solar);
        auto aVisibleRangeMaterial = std::make_shared<CMaterialSingleBand>(
          Tfvis, Tbvis, Rfvis, Rbvis, WavelengthRange::Visible);
        return std::make_shared<CMaterialDualBand>(
          aVisibleRangeMaterial, aSolarRangeMaterial, ratio);
    }

    std::shared_ptr<CMaterial> Material::dualBandMaterial(const double Tfsol,
                                                          const double Tbsol,
                                                          const double Rfsol,
                                                          const double Rbsol,
                                                          const double Tfvis,
                                                          const double Tbvis,
                                                          const double Rfvis,
                                                          const double Rbvis,
                                                          const CSeries & solarRadiation)
    {
        auto aSolarRangeMaterial =
          std::make_shared<CMaterialSingleBand>(Tfsol, Tbsol, Rfsol, Rbsol, WavelengthRange::Solar);
        auto aVisibleRangeMaterial = std::make_shared<CMaterialSingleBand>(
          Tfvis, Tbvis, Rfvis, Rbvis, WavelengthRange::Visible);
        return std::make_shared<CMaterialDualBand>(
          aVisibleRangeMaterial, aSolarRangeMaterial, solarRadiation);
    }

    std::shared_ptr<CMaterial>
      Material::dualBandBSDFMaterial(std::vector<std::vector<double>> const & Tfsol,
                                     std::vector<std::vector<double>> const & Tbsol,
                                     std::vector<std::vector<double>> const & Rfsol,
                                     std::vector<std::vector<double>> const & Rbsol,
                                     std::vector<std::vector<double>> const & Tfvis,
                                     std::vector<std::vector<double>> const & Tbvis,
                                     std::vector<std::vector<double>> const & Rfvis,
                                     std::vector<std::vector<double>> const & Rbvis,
                                     CBSDFHemisphere const & hemisphere,
                                     double ratio)
    {
        auto aSolarRangeMaterial = std::make_shared<CMaterialSingleBandBSDF>(
          Tfsol, Tbsol, Rfsol, Rbsol, hemisphere, WavelengthRange::Solar);
        auto aVisibleRangeMaterial = std::make_shared<CMaterialSingleBandBSDF>(
          Tfvis, Tbvis, Rfvis, Rbvis, hemisphere, WavelengthRange::Visible);
        return std::make_shared<CMaterialDualBandBSDF>(
          aVisibleRangeMaterial, aSolarRangeMaterial, ratio);
    }

    std::shared_ptr<CMaterial>
      Material::dualBandBSDFMaterial(std::vector<std::vector<double>> const & Tfsol,
                                     std::vector<std::vector<double>> const & Tbsol,
                                     std::vector<std::vector<double>> const & Rfsol,
                                     std::vector<std::vector<double>> const & Rbsol,
                                     std::vector<std::vector<double>> const & Tfvis,
                                     std::vector<std::vector<double>> const & Tbvis,
                                     std::vector<std::vector<double>> const & Rfvis,
                                     std::vector<std::vector<double>> const & Rbvis,
                                     CBSDFHemisphere const & hemisphere,
                                     const FenestrationCommon::CSeries & solarRadiation)
    {
        auto aSolarRangeMaterial = std::make_shared<CMaterialSingleBandBSDF>(
          Tfsol, Tbsol, Rfsol, Rbsol, hemisphere, WavelengthRange::Solar);
        auto aVisibleRangeMaterial = std::make_shared<CMaterialSingleBandBSDF>(
          Tfvis, Tbvis, Rfvis, Rbvis, hemisphere, WavelengthRange::Visible);
        return std::make_shared<CMaterialDualBandBSDF>(
          aVisibleRangeMaterial, aSolarRangeMaterial, solarRadiation);
    }

    std::shared_ptr<CMaterial> Material::singleBandMaterial(const double Tf,
                                                            const double Tb,
                                                            const double Rf,
                                                            const double Rb,
                                                            const double minLambda,
                                                            const double maxLambda)
    {
        return std::make_shared<CMaterialSingleBand>(Tf, Tb, Rf, Rb, minLambda, maxLambda);
    }

    std::shared_ptr<CMaterial>
      Material::singleBandMaterial(const double Tf,
                                   const double Tb,
                                   const double Rf,
                                   const double Rb,
                                   const FenestrationCommon::WavelengthRange range)
    {
        return std::make_shared<CMaterialSingleBand>(Tf, Tb, Rf, Rb, range);
    }

    std::shared_ptr<CMaterial> Material::nBandMaterial(
      const std::shared_ptr<SpectralAveraging::CSpectralSampleData> & measurement,
      const double thickness,
      const FenestrationCommon::MaterialType materialType,
      const FenestrationCommon::WavelengthRange range,
      const FenestrationCommon::IntegrationType integrationType,
      const double normalizationCoefficient)
    {
        auto aSample = std::make_shared<CSpectralSample>(
          measurement, CSeries(), integrationType, normalizationCoefficient);

        // Need to determine if sample is subset of allowed range in which case integration range
        // needs to be narrowed.
        auto wlRange = CWavelengthRange(range);
        auto minLambda = wlRange.minLambda();
        auto maxLambda = wlRange.maxLambda();

        const auto sampleWls = measurement->getWavelengths();
        const auto minSample = sampleWls[0];
        const auto maxSample = sampleWls[sampleWls.size() - 1];

        // Narrow down wavelengths in case sample is not measured in that range
        if(minLambda < minSample)
        {
            minLambda = minSample;
        }

        if(maxLambda > maxSample)
        {
            maxLambda = maxSample;
        }

        // Narrow down sample in case it is over limits of desired measurements
        aSample->cutExtraData(minLambda, maxLambda);

        if(aSample->getWavelengthsFromSample().empty())
        {
            throw std::runtime_error("Given measured sample does not have measurements withing "
                                     "requested range. Calculation is not possible.");
        }

        return std::make_shared<CMaterialSample>(
          aSample, thickness, materialType, minLambda, maxLambda);
    }

    std::shared_ptr<CMaterial> Material::nBandMaterial(
      const std::shared_ptr<SpectralAveraging::CSpectralSampleData> & measurement,
      double thickness,
      const FenestrationCommon::MaterialType materialType,
      const double minLambda,
      const double maxLambda,
      const FenestrationCommon::IntegrationType integrationType,
      double normalizationCoefficient)
    {
        auto aSample = std::make_shared<CSpectralSample>(
          measurement, CSeries(), integrationType, normalizationCoefficient);

        // Narrow down sample in case it is over limits of desired measurements
        aSample->cutExtraData(minLambda, maxLambda);
        if(aSample->getWavelengthsFromSample().empty())
        {
            throw std::runtime_error("Given measured sample does not have measurements withing "
                                     "requested range. Calculation is not possible.");
        }

        return std::make_shared<CMaterialSample>(
          aSample, thickness, materialType, minLambda, maxLambda);
    }

    std::shared_ptr<CMaterial> Material::nBandMaterial(
      const std::shared_ptr<SpectralAveraging::CSpectralSampleData> & measurement,
      const CSeries & detectorData,
      const double thickness,
      const FenestrationCommon::MaterialType materialType,
      const double minLambda,
      const double maxLambda,
      const FenestrationCommon::IntegrationType integrationType,
      const double normalizationCoefficient)
    {
        auto aSample = std::make_shared<CSpectralSample>(
          measurement, CSeries(), integrationType, normalizationCoefficient);
        aSample->setDetectorData(detectorData);

        // Narrow down sample in case it is over limits of desired measurements
        aSample->cutExtraData(minLambda, maxLambda);

        if(aSample->getWavelengthsFromSample().empty())
        {
            throw std::runtime_error("Given measured sample does not have measurements withing "
                                     "requested range. Calculation is not possible.");
        }

        return std::make_shared<CMaterialSample>(
          aSample, thickness, materialType, minLambda, maxLambda);
    }

    std::shared_ptr<CMaterial> Material::nBandMaterial(
      const std::shared_ptr<SpectralAveraging::CSpectralSampleData> & measurement,
      const CSeries & detectorData,
      const double thickness,
      const FenestrationCommon::MaterialType materialType,
      const FenestrationCommon::WavelengthRange t_Range,
      const FenestrationCommon::IntegrationType integrationType,
      const double normalizationCoefficient)
    {
        auto aSample = std::make_shared<CSpectralSample>(
          measurement, CSeries(), integrationType, normalizationCoefficient);

        // Need to determine if sample is subset of allowed range in which case integration range
        // needs to be narrowed.
        auto wlRange = CWavelengthRange(t_Range);
        auto minLambda = wlRange.minLambda();
        auto maxLambda = wlRange.maxLambda();

        const auto sampleWls = measurement->getWavelengths();
        const auto minSample = sampleWls[0];
        const auto maxSample = sampleWls[sampleWls.size() - 1];

        // Narrow down wavelengths in case sample is not measured in that range
        if(minLambda < minSample)
        {
            minLambda = minSample;
        }

        if(maxLambda > maxSample)
        {
            maxLambda = maxSample;
        }

        // Narrow down sample in case it is over limits of desired measurements
        aSample->cutExtraData(minLambda, maxLambda);

        if(aSample->getWavelengthsFromSample().empty())
        {
            throw std::runtime_error("Given measured sample does not have measurements withing "
                                     "requested range. Calculation is not possible.");
        }

        aSample->setDetectorData(detectorData);
        return std::make_shared<CMaterialSample>(aSample, thickness, materialType, t_Range);
    }

    std::shared_ptr<CMaterialPhotovoltaic> Material::nBandPhotovoltaicMaterial(
      const std::shared_ptr<SpectralAveraging::PhotovoltaicSampleData> & measurement,
      double thickness,
      FenestrationCommon::MaterialType materialType,
      double minLambda,
      double maxLambda,
      FenestrationCommon::IntegrationType integrationType,
      double normalizationCoefficient)
    {
        auto aSample = std::make_shared<SpectralAveraging::CPhotovoltaicSample>(
          measurement, CSeries(), integrationType, normalizationCoefficient);

        // Narrow down sample in case it is over limits of desired measurements
        aSample->cutExtraData(minLambda, maxLambda);
        if(aSample->getWavelengthsFromSample().empty())
        {
            throw std::runtime_error("Given measured sample does not have measurements withing "
                                     "requested range. Calculation is not possible.");
        }

        return std::make_shared<CMaterialPhotovoltaic>(
          aSample, thickness, materialType, minLambda, maxLambda);
    }

    std::shared_ptr<CMaterialPhotovoltaic> Material::nBandPhotovoltaicMaterial(
      const std::shared_ptr<SpectralAveraging::PhotovoltaicSampleData> & measurement,
      double thickness,
      FenestrationCommon::MaterialType materialType,
      const FenestrationCommon::WavelengthRange range,
      FenestrationCommon::IntegrationType integrationType,
      double normalizationCoefficient)
    {
        CWavelengthRange wlRange{range};
        const double minLambda{wlRange.minLambda()};
        const double maxLambda{wlRange.maxLambda()};

        return nBandPhotovoltaicMaterial(measurement,
                                         thickness,
                                         materialType,
                                         minLambda,
                                         maxLambda,
                                         integrationType,
                                         normalizationCoefficient);
    }

}   // namespace SingleLayerOptics
