#pragma once

#include <memory>
#include "WCECommon.hpp"

namespace SpectralAveraging
{
    class CSpectralSampleData;
    class PhotovoltaicSampleData;
}   // namespace SpectralAveraging

namespace SingleLayerOptics
{
    class CMaterial;
    class CMaterialPhotovoltaic;
    class CBSDFHemisphere;

    class Material
    {
    public:
        static std::shared_ptr<CMaterial> dualBandMaterial(double Tfsol,
                                                           double Tbsol,
                                                           double Rfsol,
                                                           double Rbsol,
                                                           double Tfvis,
                                                           double Tbvis,
                                                           double Rfvis,
                                                           double Rbvis);

        static std::shared_ptr<CMaterial> dualBandMaterial(double Tfsol,
                                                           double Tbsol,
                                                           double Rfsol,
                                                           double Rbsol,
                                                           double Tfvis,
                                                           double Tbvis,
                                                           double Rfvis,
                                                           double Rbvis,
                                                           double ratio);

        static std::shared_ptr<CMaterial>
          dualBandMaterial(double Tfsol,
                           double Tbsol,
                           double Rfsol,
                           double Rbsol,
                           double Tfvis,
                           double Tbvis,
                           double Rfvis,
                           double Rbvis,
                           const FenestrationCommon::CSeries & solarRadiation);

        static std::shared_ptr<CMaterial>
          dualBandBSDFMaterial(std::vector<std::vector<double>> const & Tfsol,
                               std::vector<std::vector<double>> const & Tbsol,
                               std::vector<std::vector<double>> const & Rfsol,
                               std::vector<std::vector<double>> const & Rbsol,
                               std::vector<std::vector<double>> const & Tfvis,
                               std::vector<std::vector<double>> const & Tbvis,
                               std::vector<std::vector<double>> const & Rfvis,
                               std::vector<std::vector<double>> const & Rbvis,
                               CBSDFHemisphere const & hemisphere,
                               double ratio);

        static std::shared_ptr<CMaterial>
          dualBandBSDFMaterial(std::vector<std::vector<double>> const & Tfsol,
                               std::vector<std::vector<double>> const & Tbsol,
                               std::vector<std::vector<double>> const & Rfsol,
                               std::vector<std::vector<double>> const & Rbsol,
                               std::vector<std::vector<double>> const & Tfvis,
                               std::vector<std::vector<double>> const & Tbvis,
                               std::vector<std::vector<double>> const & Rfvis,
                               std::vector<std::vector<double>> const & Rbvis,
                               CBSDFHemisphere const & hemisphere,
                               const FenestrationCommon::CSeries & solarRadiation);

        static std::shared_ptr<CMaterial> singleBandMaterial(
          double Tf, double Tb, double Rf, double Rb, double minLambda, double maxLambda);

        static std::shared_ptr<CMaterial> singleBandMaterial(
          double Tf, double Tb, double Rf, double Rb, FenestrationCommon::WavelengthRange range);

        static std::shared_ptr<CMaterial>
          nBandMaterial(const std::shared_ptr<SpectralAveraging::CSpectralSampleData> & measurement,
                        double thickness,
                        FenestrationCommon::MaterialType materialType,
                        FenestrationCommon::WavelengthRange range,
                        FenestrationCommon::IntegrationType integrationType =
                          FenestrationCommon::IntegrationType::Trapezoidal,
                        double normalizationCoefficient = 1);

        static std::shared_ptr<CMaterial>
          nBandMaterial(const std::shared_ptr<SpectralAveraging::CSpectralSampleData> & measurement,
                        double thickness,
                        FenestrationCommon::MaterialType materialType,
                        double minLambda,
                        double maxLambda,
                        FenestrationCommon::IntegrationType integrationType =
                          FenestrationCommon::IntegrationType::Trapezoidal,
                        double normalizationCoefficient = 1);

        static std::shared_ptr<CMaterial>
          nBandMaterial(const std::shared_ptr<SpectralAveraging::CSpectralSampleData> & measurement,
                        const FenestrationCommon::CSeries & detectorData,
                        const double thickness,
                        const FenestrationCommon::MaterialType materialType,
                        const double minLambda,
                        const double maxLambda,
                        const FenestrationCommon::IntegrationType integrationType =
                          FenestrationCommon::IntegrationType::Trapezoidal,
                        const double normalizationCoefficient = 1);

        static std::shared_ptr<CMaterial>
          nBandMaterial(const std::shared_ptr<SpectralAveraging::CSpectralSampleData> & measurement,
                        const FenestrationCommon::CSeries & detectorData,
                        const double thickness,
                        const FenestrationCommon::MaterialType materialType,
                        const FenestrationCommon::WavelengthRange t_Range,
                        const FenestrationCommon::IntegrationType integrationType =
                          FenestrationCommon::IntegrationType::Trapezoidal,
                        const double normalizationCoefficient = 1);

        static std::shared_ptr<CMaterialPhotovoltaic> nBandPhotovoltaicMaterial(
          const std::shared_ptr<SpectralAveraging::PhotovoltaicSampleData> & measurement,
          double thickness,
          FenestrationCommon::MaterialType materialType,
          double minLambda,
          double maxLambda,
          FenestrationCommon::IntegrationType integrationType =
            FenestrationCommon::IntegrationType::Trapezoidal,
          double normalizationCoefficient = 1);

        static std::shared_ptr<CMaterialPhotovoltaic> nBandPhotovoltaicMaterial(
          const std::shared_ptr<SpectralAveraging::PhotovoltaicSampleData> & measurement,
          double thickness,
          FenestrationCommon::MaterialType materialType,
          const FenestrationCommon::WavelengthRange range,
          FenestrationCommon::IntegrationType integrationType =
            FenestrationCommon::IntegrationType::Trapezoidal,
          double normalizationCoefficient = 1);
    };

}   // namespace SingleLayerOptics
