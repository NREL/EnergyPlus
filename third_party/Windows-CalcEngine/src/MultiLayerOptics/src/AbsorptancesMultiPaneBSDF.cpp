#include <cassert>
#include <stdexcept>

#include "AbsorptancesMultiPaneBSDF.hpp"
#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"

using namespace SingleLayerOptics;
using namespace FenestrationCommon;

namespace MultiLayerOptics
{
    CAbsorptancesMultiPaneBSDF::CAbsorptancesMultiPaneBSDF(
      Side t_Side,
      const std::shared_ptr<std::vector<double>> & t_CommonWavelengths,
      const std::shared_ptr<CSeries> & t_SolarRadiation,
      const std::shared_ptr<CBSDFLayer> & t_Layer,
      FenestrationCommon::IntegrationType t_integrator,
      double normalizationCoefficient) :
        m_CommonWavelengths(*t_CommonWavelengths),
        m_StateCalculated(false),
        m_Side(t_Side),
        m_NumOfLayers(0),
        m_Integrator(t_integrator),
        m_NormalizationCoefficient(normalizationCoefficient)
    {
        m_SolarRadiation = t_SolarRadiation->interpolate(m_CommonWavelengths);

        // Lambda matrix from spectral results. Same lambda is valid for any wavelength
        // std::shared_ptr< const std::vector< double > > aLambdas =
        // t_Layer->getResults()->lambdas();
        m_LambdaVector = t_Layer->getResults()->lambdaVector();
        m_Lambda = t_Layer->getResults()->lambdaMatrix();

        addLayer(*t_Layer);
    }

    void CAbsorptancesMultiPaneBSDF::addLayer(CBSDFLayer & t_Layer)
    {
        m_StateCalculated = false;
        m_NumOfLayers++;
        const auto aResults = t_Layer.getWavelengthResults();

        std::vector<SquareMatrix> aTausF;
        std::vector<SquareMatrix> aTausB;
        std::vector<SquareMatrix> aRhosF;
        std::vector<SquareMatrix> aRhosB;


        const auto size = m_CommonWavelengths.size();
        // loop through each wavelenght
        for(size_t i = 0; i < size; ++i)
        {
            const auto curWL = m_CommonWavelengths[i];
            const auto index = t_Layer.getBandIndex(curWL);
            assert(index > -1);
            aTausF.push_back((*aResults)[size_t(index)]->getMatrix(Side::Front, PropertySimple::T));
            aTausB.push_back((*aResults)[size_t(index)]->getMatrix(Side::Back, PropertySimple::T));
            aRhosF.push_back((*aResults)[size_t(index)]->getMatrix(Side::Front, PropertySimple::R));
            aRhosB.push_back((*aResults)[size_t(index)]->getMatrix(Side::Back, PropertySimple::R));
        }

        m_TausF.push_back(aTausF);
        m_TausB.push_back(aTausB);
        m_RhosF.push_back(aRhosF);
        m_RhosB.push_back(aRhosB);
    }

    std::vector<double> CAbsorptancesMultiPaneBSDF::Abs(const double minLambda,
                                                        const double maxLambda,
                                                        const size_t Index)
    {
        if(Index > m_TausF.size())
        {
            throw std::runtime_error("Index for glazing layer absorptance is out of range.");
        }

        const auto aLayerIndex = layerIndex(Index - 1);

        if(!m_StateCalculated)
        {
            calculateState(minLambda, maxLambda);
            m_StateCalculated = true;
        }

        return m_Abs[aLayerIndex];
    }

    std::vector<double> CAbsorptancesMultiPaneBSDF::multVectors(const std::vector<double> & t_vec1,
                                                                const std::vector<double> & t_vec2)
    {
        if(t_vec1.size() != t_vec2.size())
        {
            throw std::runtime_error("Vectors are not same size.");
        }
        std::vector<double> Result;
        for(size_t i = 0; i < t_vec1.size(); ++i)
        {
            auto value = t_vec1[i] * t_vec2[i];
            Result.push_back(value);
        }
        return Result;
    }

    std::vector<double> CAbsorptancesMultiPaneBSDF::divVectors(const std::vector<double> & t_vec1,
                                                               const std::vector<double> & t_vec2)
    {
        if(t_vec1.size() != t_vec2.size())
        {
            throw std::runtime_error("Vectors are not same size.");
        }
        std::vector<double> Result;
        for(size_t i = 0; i < t_vec1.size(); ++i)
        {
            auto value = t_vec1[i] / t_vec2[i];
            Result.push_back(value);
        }
        return Result;
    }

    std::vector<double> CAbsorptancesMultiPaneBSDF::addVectors(const std::vector<double> & t_vec1,
                                                               const std::vector<double> & t_vec2)
    {
        if(t_vec1.size() != t_vec2.size())
        {
            throw std::runtime_error("Vectors are not same size.");
        }
        std::vector<double> Result;
        for(size_t i = 0; i < t_vec1.size(); ++i)
        {
            auto value = t_vec1[i] + t_vec2[i];
            Result.push_back(value);
        }
        return Result;
    }

    void CAbsorptancesMultiPaneBSDF::calculateState(const double minLambda, const double maxLambda)
    {
        const auto numOfWavelengths = m_CommonWavelengths.size();
        auto matrixSize = m_TausF[0][0].size();

        // calculation of forward r and t coefficients
        for(size_t i = m_NumOfLayers; i-- > 0;)
        {
            // r and t for current layer (number of wavelengths)
            SquareMatrices r;
            SquareMatrices t;

            SquareMatrices vTauF;
            SquareMatrices vTauB;
            SquareMatrices vRhoF;
            SquareMatrices vRhoB;

            const size_t aLayerIndex = layerIndex(i);

            switch(m_Side)
            {
                case Side::Front:
                    vTauF = m_TausF[aLayerIndex];
                    vTauB = m_TausB[aLayerIndex];
                    vRhoF = m_RhosF[aLayerIndex];
                    vRhoB = m_RhosB[aLayerIndex];
                    break;
                case Side::Back:
                    vTauF = m_TausB[aLayerIndex];
                    vTauB = m_TausF[aLayerIndex];
                    vRhoF = m_RhosB[aLayerIndex];
                    vRhoB = m_RhosF[aLayerIndex];
                    break;
                default:
                    assert("Incorrect side selection.");
                    break;
            }

            for(size_t j = 0; j < numOfWavelengths; ++j)
            {
                auto aTauF = vTauF[j];
                auto aTauB = vTauB[j];
                auto aRhoF = vRhoF[j];
                auto aRhoB = vRhoB[j];

                auto aRi = m_Lambda * aRhoF;

                if(i != m_NumOfLayers - 1)
                {
                    const auto prevR = m_rCoeffs[i][j];
                    const auto lambdaTauF = m_Lambda * aTauF;
                    const auto Denominator = getDenomForRTCoeff(aRhoB, prevR);
                    auto rsecF = lambdaTauF * lambdaTauF;
                    rsecF = rsecF * prevR;
                    rsecF = rsecF * Denominator;
                    aRi = aRi + rsecF;

                    const auto tfwd = lambdaTauF * Denominator;
                    t.push_back(tfwd);
                }
                else
                {
                    t.push_back(m_Lambda * aTauF);
                }

                r.push_back(aRi);
            }
            m_rCoeffs.insert(m_rCoeffs.begin(), r);
            m_tCoeffs.insert(m_tCoeffs.begin(), t);
        }

        // For every layer-wavelength set there is a set of incoming/outgoing directions
        std::vector<SquareMatrices> IminusM(m_NumOfLayers);
        std::vector<SquareMatrices> IplusM(m_NumOfLayers);

        for(size_t i = 0; i < m_NumOfLayers; ++i)
        {
            IminusM[i].resize(numOfWavelengths);
            IplusM[i].resize(numOfWavelengths);
        }

        // This is true for every incoming wavelength
        SquareMatrix Iincoming(matrixSize);
        Iincoming.setIdentity();

        // calculation irradiances (normalized to 1)
        for(size_t i = 0; i < m_NumOfLayers; ++i)
        {
            for(size_t j = 0; j < numOfWavelengths; ++j)
            {
                const auto r = m_rCoeffs[i][j];
                const auto t = m_tCoeffs[i][j];
                SquareMatrix activeI;
                if(i == 0)
                {
                    activeI = Iincoming;
                }
                else
                {
                    activeI = IminusM[i - 1][j];
                }

                IplusM[i][j] = r * activeI;
                IminusM[i][j] = t * activeI;
            }
        }

        // Convert to incoming/outgoing energy for every direction
        std::vector<std::vector<std::vector<double>>> IminusV(m_NumOfLayers);
        std::vector<std::vector<std::vector<double>>> IplusV(m_NumOfLayers);

        for(size_t i = 0; i < m_NumOfLayers; ++i)
        {
            IminusV[i].resize(numOfWavelengths);
            IplusV[i].resize(numOfWavelengths);
        }

        std::vector<double> One(matrixSize, 1);

        for(size_t j = 0; j < m_NumOfLayers; ++j)
        {
            for(size_t k = 0; k < m_CommonWavelengths.size(); ++k)
            {
                IminusV[j][k] = One * IminusM[j][k];
                IplusV[j][k] = One * IplusM[j][k];
            }
        }

        m_Abs.clear();
        m_Abs.resize(m_NumOfLayers);

        for(size_t i = 0; i < m_NumOfLayers; ++i)
        {
            m_Abs[i].resize(matrixSize);
        }

        const auto totalSolar =
          m_SolarRadiation.integrate(m_Integrator, m_NormalizationCoefficient)
            ->sum(minLambda, maxLambda);

        // calculation of solar absorptances
        for(size_t i = 0; i < matrixSize; ++i)
        {
            for(size_t j = 0; j < m_NumOfLayers; ++j)
            {
                std::shared_ptr<CSeries> curSpectralProperties = std::make_shared<CSeries>();
                for(size_t k = 0; k < m_CommonWavelengths.size(); ++k)
                {
                    double IminusIncoming = 0;
                    double IminusOutgoing = 0;
                    double IplusIncoming = 0;
                    double IplusOutgoing = 0;

                    IminusOutgoing = IminusV[j][k][i];
                    IplusOutgoing = IplusV[j][k][i];
                    if(j == 0)
                    {
                        IminusIncoming = 1;
                    }
                    else
                    {
                        IminusIncoming = (IminusV[j - 1][k])[i];
                    }

                    if(j == m_NumOfLayers - 1)
                    {
                        IplusIncoming = 0;
                    }
                    else
                    {
                        IplusIncoming = (IplusV[j + 1][k])[i];
                    }

                    const auto absValue =
                      IminusIncoming + IplusIncoming - IminusOutgoing - IplusOutgoing;
                    curSpectralProperties->addProperty(m_CommonWavelengths[k], absValue);
                }

                const CSeries absorbedIrradiance = (*curSpectralProperties) * m_SolarRadiation;
                const CSeries integratedAbsorbed =
                  *absorbedIrradiance.integrate(m_Integrator, m_NormalizationCoefficient);
                double value = integratedAbsorbed.sum(minLambda, maxLambda);
                value = value / totalSolar;
                m_Abs[j][i] = value;
            }
        }

        m_StateCalculated = true;
    }

    SquareMatrix
      CAbsorptancesMultiPaneBSDF::getDenomForRTCoeff(const SquareMatrix & t_Reflectance,
                                                     const SquareMatrix & t_PreviousR) const
    {
        const size_t matrixSize = t_Reflectance.size();
        SquareMatrix denominator(matrixSize);
        denominator.setIdentity();
        SquareMatrix lambdaRf = m_Lambda * t_Reflectance;
        lambdaRf = lambdaRf * t_PreviousR;
        denominator = denominator - lambdaRf;
        denominator = denominator.inverse();
        return denominator;
    }

    size_t CAbsorptancesMultiPaneBSDF::layerIndex(const size_t Index) const
    {
        size_t aLayerIndex = 0;
        switch(m_Side)
        {
            case Side::Front:
                aLayerIndex = Index;
                break;
            case Side::Back:
                aLayerIndex = m_NumOfLayers - Index - 1;
                break;
            default:
                assert("Incorrect side selection.");
                break;
        }
        return aLayerIndex;
    }

}   // namespace MultiLayerOptics
