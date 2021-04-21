#ifndef BSDFDIRECTIONS_H
#define BSDFDIRECTIONS_H

#include <vector>
#include <memory>
#include <map>

#include "WCECommon.hpp"
#include "BSDFPatch.hpp"

namespace SingleLayerOptics
{
    class CBeamDirection;

    class CBSDFDefinition
    {
    public:
        CBSDFDefinition(double t_Theta, size_t t_NumOfPhis);
        double theta() const;
        size_t numOfPhis() const;

    private:
        double m_Theta;
        size_t m_NumOfPhis;
    };

    enum class BSDFDirection
    {
        Incoming,
        Outgoing
    };

    class CBSDFDirections
    {
    public:
        CBSDFDirections(const std::vector<CBSDFDefinition> & t_Definitions, BSDFDirection t_Side);
        size_t size() const;
        const CBSDFPatch & operator[]( size_t Index ) const;
        std::vector<CBSDFPatch>::iterator begin();
        std::vector<CBSDFPatch>::iterator end();

        std::vector<double> lambdaVector() const;
        const FenestrationCommon::SquareMatrix & lambdaMatrix() const;

        // returns index of element that is closest to given Theta and Phi angles
        size_t getNearestBeamIndex(double t_Theta, double t_Phi) const;

    private:
        std::vector<CBSDFPatch> m_Patches;
        std::vector<double> m_LambdaVector;
        FenestrationCommon::SquareMatrix m_LambdaMatrix;
    };

    enum class BSDFBasis
    {
        Small,
        Quarter,
        Half,
        Full
    };

    class CBSDFHemisphere
    {
    public:
        static CBSDFHemisphere create(BSDFBasis t_Basis);
        static CBSDFHemisphere create(const std::vector<CBSDFDefinition> & t_Definitions);

        const CBSDFDirections & getDirections(BSDFDirection t_Side) const;

    private:
        // Construction for pre-defined basis
        explicit CBSDFHemisphere(BSDFBasis t_Basis);
        // Construction for custom basis
        explicit CBSDFHemisphere(const std::vector<CBSDFDefinition> & t_Definitions);

        std::map<BSDFDirection, CBSDFDirections> m_Directions;
    };

}   // namespace SingleLayerOptics

#endif
