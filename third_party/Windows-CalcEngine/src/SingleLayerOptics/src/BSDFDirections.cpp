#include <cassert>
#include <algorithm>

#include "BSDFDirections.hpp"
#include "BSDFPatch.hpp"
#include "BSDFThetaLimits.hpp"
#include "BSDFPhiLimits.hpp"
#include "WCECommon.hpp"

using namespace FenestrationCommon;

namespace SingleLayerOptics
{
    /////////////////////////////////////////////////////////////////
    ///  CBSDFDefinition
    /////////////////////////////////////////////////////////////////

    CBSDFDefinition::CBSDFDefinition(const double t_Theta, const size_t t_NumOfPhis) :
        m_Theta(t_Theta),
        m_NumOfPhis(t_NumOfPhis)
    {}

    double CBSDFDefinition::theta() const
    {
        return m_Theta;
    }

    size_t CBSDFDefinition::numOfPhis() const
    {
        return m_NumOfPhis;
    }

    /////////////////////////////////////////////////////////////////
    ///  CBSDFDirections
    /////////////////////////////////////////////////////////////////

    CBSDFDirections::CBSDFDirections(const std::vector<CBSDFDefinition> & t_Definitions,
                                     const BSDFDirection t_Side)
    {
        std::vector<double> thetaAngles;
        std::vector<size_t> numPhiAngles;
        for(auto it = t_Definitions.begin(); it < t_Definitions.end(); ++it)
        {
            thetaAngles.push_back((*it).theta());
            numPhiAngles.push_back((*it).numOfPhis());
        }

        CThetaLimits ThetaLimits(thetaAngles);
        std::vector<double> thetaLimits = *ThetaLimits.getThetaLimits();

        double lowerTheta = thetaLimits[0];
        for(size_t i = 1; i < thetaLimits.size(); ++i)
        {
            double upperTheta = thetaLimits[i];
            std::shared_ptr<CAngleLimits> currentTheta = nullptr;
            if(i == 1)
            {
                currentTheta = std::make_shared<CCentralAngleLimits>(upperTheta);
            }
            else
            {
                currentTheta = std::make_shared<CAngleLimits>(lowerTheta, upperTheta);
            }


			const auto nPhis = numPhiAngles[i - 1];
            CPhiLimits phiAngles(nPhis);
            auto phiLimits = phiAngles.getPhiLimits();
            double lowerPhi = phiLimits[0];
            if(t_Side == BSDFDirection::Outgoing && nPhis != 1)
            {
                lowerPhi += 180;
            }
            for(size_t j = 1; j < phiLimits.size(); ++j)
            {
                double upperPhi = phiLimits[j];
                if(t_Side == BSDFDirection::Outgoing && nPhis != 1)
                {
                    upperPhi += 180;
                }
                CAngleLimits currentPhi(lowerPhi, upperPhi);
                CBSDFPatch currentPatch(currentTheta, currentPhi);
                m_Patches.push_back(currentPatch);
                lowerPhi = upperPhi;
            }
            lowerTheta = upperTheta;
        }

        // build lambda std::vector and matrix
        size_t size = m_Patches.size();
        // m_LambdaVector = std::make_shared<std::vector<double>>();
        m_LambdaMatrix = SquareMatrix(size);
        for(size_t i = 0; i < size; ++i)
        {
            m_LambdaVector.push_back(m_Patches[i].lambda());
            m_LambdaMatrix(i, i) = m_Patches[i].lambda();
        }
    }

    size_t CBSDFDirections::size() const
    {
        return m_Patches.size();
    }

    const CBSDFPatch & CBSDFDirections::operator[]( size_t Index ) const
    {
        return m_Patches[Index];
    }

    std::vector<CBSDFPatch>::iterator CBSDFDirections::begin()
    {
        return m_Patches.begin();
    }

    std::vector<CBSDFPatch>::iterator CBSDFDirections::end()
    {
        return m_Patches.end();
    }

    std::vector<double> CBSDFDirections::lambdaVector() const
    {
        return m_LambdaVector;
    }

    const SquareMatrix & CBSDFDirections::lambdaMatrix() const
    {
        return m_LambdaMatrix;
    }

    size_t CBSDFDirections::getNearestBeamIndex(const double t_Theta, const double t_Phi) const
    {
        auto it = std::find_if(
          m_Patches.begin(), m_Patches.end(), [&](const CBSDFPatch & a) {
              return a.isInPatch(t_Theta, t_Phi);
          });

		if(it == m_Patches.end())
		{
			throw std::runtime_error("Could not find nearest beam index");
		}

        size_t index = size_t(std::distance(m_Patches.begin(), it));
        return index;
    }

    /////////////////////////////////////////////////////////////////
    ///  CBSDFHemisphere
    /////////////////////////////////////////////////////////////////

    CBSDFHemisphere::CBSDFHemisphere(const BSDFBasis t_Basis)
    {
        std::vector<CBSDFDefinition> aDefinitions;
        switch(t_Basis)
        {
            case BSDFBasis::Small:
                aDefinitions = {{0, 1}, {13, 1}, {26, 1}, {39, 1}, {52, 1}, {65, 1}, {80.75, 1}};
                break;
            case BSDFBasis::Quarter:
                aDefinitions = {{0, 1}, {18, 8}, {36, 12}, {54, 12}, {76.5, 8}};
                break;
            case BSDFBasis::Half:
                aDefinitions = {
                  {0, 1}, {13, 8}, {26, 12}, {39, 16}, {52, 20}, {65, 12}, {80.75, 8}};
                break;
            case BSDFBasis::Full:
                aDefinitions = {{0, 1},
                                {10, 8},
                                {20, 16},
                                {30, 20},
                                {40, 24},
                                {50, 24},
                                {60, 24},
                                {70, 16},
                                {82.5, 12}};
                break;
            default:
                throw std::runtime_error("Incorrect definition of the basis.");
        }
        m_Directions.insert(std::make_pair(BSDFDirection::Incoming,
                                           CBSDFDirections(aDefinitions, BSDFDirection::Incoming)));
        m_Directions.insert(std::make_pair(BSDFDirection::Outgoing,
                                           CBSDFDirections(aDefinitions, BSDFDirection::Outgoing)));
    }

    CBSDFHemisphere::CBSDFHemisphere(const std::vector<CBSDFDefinition> & t_Definitions) :
        m_Directions(
          {{BSDFDirection::Incoming, CBSDFDirections(t_Definitions, BSDFDirection::Incoming)},
           {BSDFDirection::Outgoing, CBSDFDirections(t_Definitions, BSDFDirection::Outgoing)}})
    {}

    const CBSDFDirections & CBSDFHemisphere::getDirections(const BSDFDirection tDirection) const
    {
        return m_Directions.at(tDirection);
    }

    CBSDFHemisphere CBSDFHemisphere::create(BSDFBasis t_Basis)
    {
        return CBSDFHemisphere(t_Basis);
    }

    CBSDFHemisphere CBSDFHemisphere::create(const std::vector<CBSDFDefinition> & t_Definitions)
    {
        return CBSDFHemisphere(t_Definitions);
    }

}   // namespace SingleLayerOptics
