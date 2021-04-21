#ifndef BSDFINTEGRATOR_H
#define BSDFINTEGRATOR_H

#include <memory>
#include <vector>
#include <map>
#include <WCECommon.hpp>

#include "BSDFDirections.hpp"

namespace FenestrationCommon
{
    class SquareMatrix;
    enum class Side;
    enum class PropertySimple;

}   // namespace FenestrationCommon

namespace SingleLayerOptics
{
    typedef std::pair<FenestrationCommon::Side, FenestrationCommon::PropertySimple> pair_Side_PropertySimple;

    // Layer results from BSDF directions.
    class CBSDFIntegrator
    {
    public:
        explicit CBSDFIntegrator(const std::shared_ptr<const CBSDFIntegrator> & t_Integrator);
        explicit CBSDFIntegrator(const CBSDFDirections & t_Directions);

        // Result matrices
        FenestrationCommon::SquareMatrix & getMatrix(FenestrationCommon::Side t_Side,
            FenestrationCommon::PropertySimple t_Property);

        [[nodiscard]] const FenestrationCommon::SquareMatrix & at(FenestrationCommon::Side t_Side,
            FenestrationCommon::PropertySimple t_Property) const;

        void setResultMatrices(const FenestrationCommon::SquareMatrix & t_Tau,
                               const FenestrationCommon::SquareMatrix & t_Rho,
                               FenestrationCommon::Side t_Side);

        // Direct-direct components
        double DirDir(FenestrationCommon::Side t_Side,
                      FenestrationCommon::PropertySimple t_Property,
                      double t_Theta = 0,
                      double t_Phi = 0) const;
        double DirDir(FenestrationCommon::Side t_Side,
        	FenestrationCommon::PropertySimple t_Property, size_t Index) const;

        // Directional hemispherical results for every direction in BSDF definition
        std::vector<double> DirHem(FenestrationCommon::Side t_Side,
        	FenestrationCommon::PropertySimple t_Property);
        std::vector<double> Abs(FenestrationCommon::Side t_Side);

        // Directional hemispherical results for given Theta and Phi direction
        double DirHem(FenestrationCommon::Side t_Side,
                      FenestrationCommon::PropertySimple t_Property,
                      double t_Theta,
                      double t_Phi);
        double Abs(FenestrationCommon::Side t_Side, double t_Theta, double t_Phi);
        double Abs(FenestrationCommon::Side t_Side, size_t Index);

        // std::shared_ptr< const CBSDFDirections > getDirections() const;

        double DiffDiff(FenestrationCommon::Side t_Side, FenestrationCommon::PropertySimple t_Property);

        double AbsDiffDiff(FenestrationCommon::Side t_Side);

        // Lambda values for the layer.
        std::vector<double> lambdaVector() const;
        FenestrationCommon::SquareMatrix lambdaMatrix() const;

        size_t getNearestBeamIndex(double t_Theta, double t_Phi) const;

    protected:
        const CBSDFDirections m_Directions;
        size_t m_DimMatrices;

    private:
        // Hemispherical integration over m_Directions
        double integrate(FenestrationCommon::SquareMatrix const & t_Matrix) const;

        void calcDiffuseDiffuse();
        void calcHemispherical();

        std::map<pair_Side_PropertySimple, FenestrationCommon::SquareMatrix> m_Matrix;
        std::map<pair_Side_PropertySimple, std::vector<double>> m_Hem;
        std::map<FenestrationCommon::Side, std::vector<double>> m_Abs;

        bool m_HemisphericalCalculated;
        bool m_DiffuseDiffuseCalculated;
        FenestrationCommon::mmap<double, FenestrationCommon::Side, FenestrationCommon::PropertySimple> m_MapDiffDiff;
    };

}   // namespace SingleLayerOptics

#endif
