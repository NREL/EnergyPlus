#ifndef MULTIBSDFLAYER_H
#define MULTIBSDFLAYER_H

#include <memory>
#include <vector>
#include <map>
#include <WCECommon.hpp>
#include <WCESingleLayerOptics.hpp>

#include "EquivalentBSDFLayer.hpp"

namespace FenestrationCommon
{
    class CSeries;

    class SquareMatrix;

    enum class Side;
    enum class PropertySimple;

}   // namespace FenestrationCommon

namespace SingleLayerOptics
{
    class CBSDFIntegrator;
}

namespace MultiLayerOptics
{
    class CEquivalentBSDFLayer;

    typedef std::shared_ptr<std::vector<FenestrationCommon::CSeries>> p_VectorSeries;

    class CMultiPaneBSDF : public SingleLayerOptics::IScatteringLayer
    {
    public:
        static std::unique_ptr<CMultiPaneBSDF>
          create(const std::shared_ptr<SingleLayerOptics::CBSDFLayer> & t_Layer,
                 const FenestrationCommon::CSeries & t_SolarRadiation,
                 const std::vector<double> & t_CommonWavelengths);

        static std::unique_ptr<CMultiPaneBSDF>
          create(const std::shared_ptr<SingleLayerOptics::CBSDFLayer> & t_Layer,
                 const FenestrationCommon::CSeries & t_SolarRadiation,
                 const FenestrationCommon::CSeries & t_DetectorData,
                 const std::vector<double> & t_CommonWavelengths);

        static std::unique_ptr<CMultiPaneBSDF>
          create(const std::vector<std::shared_ptr<SingleLayerOptics::CBSDFLayer>> & t_Layers,
                 const FenestrationCommon::CSeries & t_SolarRadiation,
                 const std::vector<double> & t_CommonWavelengths);

        static std::unique_ptr<CMultiPaneBSDF>
          create(const std::vector<std::shared_ptr<SingleLayerOptics::CBSDFLayer>> & t_Layers,
                 const FenestrationCommon::CSeries & t_SolarRadiation,
                 const FenestrationCommon::CSeries & t_DetectorData,
                 const std::vector<double> & t_CommonWavelengths);

        static std::unique_ptr<CMultiPaneBSDF>
          create(const std::shared_ptr<SingleLayerOptics::CBSDFLayer> & t_Layer,
                 const FenestrationCommon::CSeries & t_SolarRadiation);

        static std::unique_ptr<CMultiPaneBSDF>
          create(const std::shared_ptr<SingleLayerOptics::CBSDFLayer> & t_Layer,
                 const FenestrationCommon::CSeries & t_SolarRadiation,
                 const FenestrationCommon::CSeries & t_DetectorData);

        static std::unique_ptr<CMultiPaneBSDF>
          create(const std::vector<std::shared_ptr<SingleLayerOptics::CBSDFLayer>> & t_Layers,
                 const FenestrationCommon::CSeries & t_SolarRadiation);

        static std::unique_ptr<CMultiPaneBSDF>
          create(const std::vector<std::shared_ptr<SingleLayerOptics::CBSDFLayer>> & t_Layers,
                 const FenestrationCommon::CSeries & t_SolarRadiation,
                 const FenestrationCommon::CSeries & t_DetectorData);

        void setIntegrationType(FenestrationCommon::IntegrationType t_type,
                                double normalizationCoefficient);

        void addLayer(const std::shared_ptr<SingleLayerOptics::CBSDFLayer> & t_Layer);

        // Whole matrix results
        FenestrationCommon::SquareMatrix getMatrix(double minLambda,
                                                   double maxLambda,
                                                   FenestrationCommon::Side t_Side,
                                                   FenestrationCommon::PropertySimple t_Property);

        double getPropertySimple(const double minLambda,
                                 const double maxLambda,
                                 const FenestrationCommon::PropertySimple t_Property,
                                 const FenestrationCommon::Side t_Side,
                                 const FenestrationCommon::Scattering t_Scattering,
                                 const double t_Theta = 0,
                                 const double t_Phi = 0) override;

        double DirDir(double minLambda,
                      double maxLambda,
                      FenestrationCommon::Side t_Side,
                      FenestrationCommon::PropertySimple t_Property,
                      double t_Theta,
                      double t_Phi);

        double DirDir(double minLambda,
                      double maxLambda,
                      FenestrationCommon::Side t_Side,
                      FenestrationCommon::PropertySimple t_Property,
                      size_t Index);

        // std::vector of layer by layer absorptances for each incoming direction
        std::vector<double> &
          Abs(double minLambda, double maxLambda, FenestrationCommon::Side t_Side, size_t Index);

        std::vector<double> getAbsorptanceLayers(double minLambda,
                                                 double maxLambda,
                                                 FenestrationCommon::Side side,
                                                 FenestrationCommon::ScatteringSimple scattering,
                                                 double theta = 0,
                                                 double phi = 0) override;

        // Hemispherical results for every direction
        std::vector<double> DirHem(double minLambda,
                                   double maxLambda,
                                   FenestrationCommon::Side t_Side,
                                   FenestrationCommon::PropertySimple t_Property);

        // Directional hemispherical results for given Theta and Phi direction
        double DirHem(double minLambda,
                      double maxLambda,
                      FenestrationCommon::Side t_Side,
                      FenestrationCommon::PropertySimple t_Property,
                      double t_Theta,
                      double t_Phi);

        // This function will return portion of property that goes only as diffuse part. In other
        // words, this will be DirHemispherical - DirDir.
        double DirDiff(double minLambda,
                       double maxLambda,
                       FenestrationCommon::Side t_Side,
                       FenestrationCommon::PropertySimple t_Property,
                       double t_Theta,
                       double t_Phi);

        double DirHem(double minLambda,
                      double maxLambda,
                      FenestrationCommon::Side t_Side,
                      FenestrationCommon::PropertySimple t_Property,
                      size_t Index);

        double Abs(double minLambda,
                   double maxLambda,
                   FenestrationCommon::Side t_Side,
                   size_t layerIndex,
                   double t_Theta,
                   double t_Phi);

        double Abs(double minLambda,
                   double maxLambda,
                   FenestrationCommon::Side t_Side,
                   size_t layerIndex,
                   size_t beamIndex);

        // Diffuse to diffuse properties
        double DiffDiff(double minLambda,
                        double maxLambda,
                        FenestrationCommon::Side t_Side,
                        FenestrationCommon::PropertySimple t_Property);

        double AbsDiff(double minLambda,
                       double maxLambda,
                       FenestrationCommon::Side t_Side,
                       size_t t_LayerIndex);

        // Energy that gets transmitted or reflected from certain direction
        double energy(double minLambda,
                      double maxLambda,
                      FenestrationCommon::Side t_Side,
                      FenestrationCommon::PropertySimple t_Property,
                      double t_Theta,
                      double t_Phi);

        double energyAbs(double minLambda,
                         double maxLambda,
                         FenestrationCommon::Side t_Side,
                         size_t Index,
                         double t_Theta,
                         double t_Phi);

        std::vector<double> getWavelengths() const override;

        double getMinLambda() const override;
        double getMaxLambda() const override;

    private:
        CMultiPaneBSDF(const std::vector<std::shared_ptr<SingleLayerOptics::CBSDFLayer>> & t_Layer,
                       const FenestrationCommon::CSeries & t_SolarRadiation,
                       const std::vector<double> & t_CommonWavelengths);

        CMultiPaneBSDF(const std::vector<std::shared_ptr<SingleLayerOptics::CBSDFLayer>> & t_Layer,
                       const FenestrationCommon::CSeries & t_SolarRadiation,
                       const FenestrationCommon::CSeries & t_DetectorData,
                       const std::vector<double> & t_CommonWavelengths);

        CMultiPaneBSDF(const std::vector<std::shared_ptr<SingleLayerOptics::CBSDFLayer>> & t_Layer,
                       const FenestrationCommon::CSeries & t_SolarRadiation);

        CMultiPaneBSDF(const std::vector<std::shared_ptr<SingleLayerOptics::CBSDFLayer>> & t_Layer,
                       const FenestrationCommon::CSeries & t_DetectorData,
                       const FenestrationCommon::CSeries & t_SolarRadiation);

        void initialize(
          const std::vector<std::shared_ptr<SingleLayerOptics::CBSDFLayer>> & t_Layer,
          const FenestrationCommon::CSeries & t_SolarRadiation,
          const FenestrationCommon::CSeries & t_DetectorData = FenestrationCommon::CSeries());

        void calculate(double minLambda, double maxLambda);

        void calcHemisphericalAbs(FenestrationCommon::Side t_Side);

        [[nodiscard]] std::vector<double> getCommonWavelengths(
          const std::vector<std::shared_ptr<SingleLayerOptics::CBSDFLayer>> & t_Layer) const;

        CEquivalentBSDFLayer m_Layer;

        // Solar radiation for initialization
        FenestrationCommon::CSeries m_SolarRadiationInit;

        p_VectorSeries m_IncomingSpectra;
        std::vector<double> m_IncomingSolar;

        std::shared_ptr<SingleLayerOptics::CBSDFIntegrator> m_Results;

        std::map<FenestrationCommon::Side, std::vector<std::vector<double>>> m_Abs;

        // Hemispherical absorptances for every layer
        std::map<FenestrationCommon::Side, std::shared_ptr<std::vector<double>>> m_AbsHem;

        bool m_Calculated;
        double m_MinLambdaCalculated;
        double m_MaxLambdaCalculated;

        FenestrationCommon::IntegrationType m_Integrator;
        double m_NormalizationCoefficient;
    };

}   // namespace MultiLayerOptics

#endif
