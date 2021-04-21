#ifndef AbsorptancesMultiPaneBSDF_H
#define AbsorptancesMultiPaneBSDF_H

#include <memory>
#include <vector>
#include "WCECommon.hpp"

namespace SingleLayerOptics
{
    class CBSDFLayer;

}

namespace MultiLayerOptics
{
    typedef std::vector<FenestrationCommon::SquareMatrix> SquareMatrices;

    // Calculate BSDF absorptances of MultiLayerOptics layer.
    class CAbsorptancesMultiPaneBSDF
    {
    public:
        CAbsorptancesMultiPaneBSDF(FenestrationCommon::Side t_Side,
                                   const std::shared_ptr<std::vector<double>> & t_CommonWavelengths,
                                   const std::shared_ptr<FenestrationCommon::CSeries> & t_SolarRadiation,
                                   const std::shared_ptr<SingleLayerOptics::CBSDFLayer> & t_Layer,
                                   FenestrationCommon::IntegrationType t_integrator,
                                   double normalizationCoefficient);

        void addLayer(SingleLayerOptics::CBSDFLayer & t_Layer);

        std::vector<double> Abs(const double minLambda, const double maxLambda, const size_t Index);

    private:
        void calculateState(const double minLambda, const double maxLambda);

        // Denominator term for t and r coefficients (absorptance calculations)
        FenestrationCommon::SquareMatrix getDenomForRTCoeff(const FenestrationCommon::SquareMatrix & t_Reflectance,
                                                            const FenestrationCommon::SquareMatrix & t_PreviousR) const;

        // Returns correct layer index. Depends if object is used to calculate forward or backward properties
        size_t layerIndex(const size_t Index) const;

        static std::vector<double> multVectors(const std::vector<double> & t_vec1, const std::vector<double> & t_vec2);
        static std::vector<double> divVectors(const std::vector<double> & t_vec1, const std::vector<double> & t_vec2);
        static std::vector<double> addVectors(const std::vector<double> & t_vec1, const std::vector<double> & t_vec2);

        FenestrationCommon::SquareMatrix m_Lambda;
        std::vector<double> m_LambdaVector;

        FenestrationCommon::CSeries m_SolarRadiation;

        // Layer by layer coefficients for each wavelength (layer, wavelength, direction)
        std::vector<SquareMatrices> m_TausF;
        std::vector<SquareMatrices> m_TausB;
        std::vector<SquareMatrices> m_RhosF;
        std::vector<SquareMatrices> m_RhosB;

        std::vector<SquareMatrices> m_rCoeffs;
        std::vector<SquareMatrices> m_tCoeffs;

        // Absorptances for each direction
        std::vector<std::vector<double>> m_Abs;

        std::vector<double> m_CommonWavelengths;
        bool m_StateCalculated;
        FenestrationCommon::Side m_Side;
        size_t m_NumOfLayers;

        FenestrationCommon::IntegrationType m_Integrator;
        double m_NormalizationCoefficient;
    };
}   // namespace MultiLayerOptics

#endif
