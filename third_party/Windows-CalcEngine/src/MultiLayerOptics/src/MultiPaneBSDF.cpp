#include <cmath>
#include <numeric>
#include <algorithm>
#include <cassert>

#include "MultiPaneBSDF.hpp"
#include "EquivalentBSDFLayer.hpp"
#include "EquivalentBSDFLayerSingleBand.hpp"
#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"

using namespace FenestrationCommon;
using namespace SingleLayerOptics;

namespace MultiLayerOptics
{
    CMultiPaneBSDF::CMultiPaneBSDF(
      const std::vector<std::shared_ptr<SingleLayerOptics::CBSDFLayer>> & t_Layer,
      const FenestrationCommon::CSeries & t_SolarRadiation,
      const FenestrationCommon::CSeries & t_DetectorData,
      const std::vector<double> & t_CommonWavelengths) :
        m_Layer(t_CommonWavelengths, t_Layer[0]),
        m_Results(
          std::make_shared<CBSDFIntegrator>(t_Layer[0]->getDirections(BSDFDirection::Incoming))),
        m_Calculated(false),
        m_MinLambdaCalculated(0),
        m_MaxLambdaCalculated(0),
        m_Integrator(IntegrationType::Trapezoidal),
        m_NormalizationCoefficient(1)
    {
        initialize(t_Layer, t_SolarRadiation, t_DetectorData);
    }

    CMultiPaneBSDF::CMultiPaneBSDF(
      const std::vector<std::shared_ptr<SingleLayerOptics::CBSDFLayer>> & t_Layer,
      const FenestrationCommon::CSeries & t_SolarRadiation,
      const std::vector<double> & t_CommonWavelengths) :
        m_Layer(t_CommonWavelengths, t_Layer[0]),
        m_Results(
          std::make_shared<CBSDFIntegrator>(t_Layer[0]->getDirections(BSDFDirection::Incoming))),
        m_Calculated(false),
        m_MinLambdaCalculated(0),
        m_MaxLambdaCalculated(0),
        m_Integrator(IntegrationType::Trapezoidal),
        m_NormalizationCoefficient(1)
    {
        initialize(t_Layer, t_SolarRadiation);
    }

    void CMultiPaneBSDF::initialize(
      const std::vector<std::shared_ptr<SingleLayerOptics::CBSDFLayer>> & t_Layer,
      const CSeries & t_SolarRadiation,
      const CSeries & t_DetectorData)
    {
        auto solarRadiation{t_SolarRadiation};
        if(t_DetectorData.size() > 0)
        {
            const auto commonWavelengths = solarRadiation.getXArray();
            solarRadiation = solarRadiation * t_DetectorData.interpolate(commonWavelengths);
        }
        m_SolarRadiationInit = solarRadiation;
        for(Side aSide : EnumSide())
        {
            this->m_AbsHem[aSide] = std::make_shared<std::vector<double>>();
        }

        // This will initialize layer material data with given spectral distribution
        this->m_Layer.setSolarRadiation(this->m_SolarRadiationInit);

        size_t directionsSize = t_Layer[0]->getDirections(BSDFDirection::Incoming).size();
        this->m_IncomingSolar.resize(directionsSize);

        // For blank incoming spectra, defaults needs to be filled into
        this->m_IncomingSpectra = std::make_shared<std::vector<FenestrationCommon::CSeries>>();
        for(size_t i = 0; i < directionsSize; ++i)
        {
            this->m_IncomingSpectra->push_back(solarRadiation);
        }

        // First layer has already been added. Must skip it here
        for(size_t j = 1u; j < t_Layer.size(); ++j)
        {
            this->addLayer(t_Layer[j]);
        }
    }

    CMultiPaneBSDF::CMultiPaneBSDF(
      const std::vector<std::shared_ptr<SingleLayerOptics::CBSDFLayer>> & t_Layer,
      const FenestrationCommon::CSeries & t_SolarRadiation) :
        CMultiPaneBSDF(t_Layer, t_SolarRadiation, t_Layer[0]->getBandWavelengths())
    {}

    CMultiPaneBSDF::CMultiPaneBSDF(
      const std::vector<std::shared_ptr<SingleLayerOptics::CBSDFLayer>> & t_Layer,
      const FenestrationCommon::CSeries & t_SolarRadiation,
      const FenestrationCommon::CSeries & t_DetectorData) :
        CMultiPaneBSDF(t_Layer, t_SolarRadiation, t_DetectorData, t_Layer[0]->getBandWavelengths())
    {}

    SquareMatrix CMultiPaneBSDF::getMatrix(const double minLambda,
                                           const double maxLambda,
                                           const Side t_Side,
                                           const PropertySimple t_Property)
    {
        calculate(minLambda, maxLambda);

        return m_Results->getMatrix(t_Side, t_Property);
    }

    double CMultiPaneBSDF::DirDir(const double minLambda,
                                  const double maxLambda,
                                  const Side t_Side,
                                  const PropertySimple t_Property,
                                  const double t_Theta,
                                  const double t_Phi)
    {
        calculate(minLambda, maxLambda);

        return m_Results->DirDir(t_Side, t_Property, t_Theta, t_Phi);
    }

    double CMultiPaneBSDF::DirDir(const double minLambda,
                                  const double maxLambda,
                                  const Side t_Side,
                                  const PropertySimple t_Property,
                                  const size_t Index)
    {
        calculate(minLambda, maxLambda);

        return m_Results->DirDir(t_Side, t_Property, Index);
    }

    void CMultiPaneBSDF::calculate(const double minLambda, const double maxLambda)
    {
        if(!m_Calculated || minLambda != m_MinLambdaCalculated
           || maxLambda != m_MaxLambdaCalculated)
        {
            m_IncomingSolar.clear();

            for(CSeries & aSpectra : *m_IncomingSpectra)
            {
                // each incoming spectra must be intepolated to same wavelengths as this IGU is
                // using
                aSpectra = aSpectra.interpolate(m_Layer.getCommonWavelengths());

                CSeries iTotalSolar = *aSpectra.integrate(m_Integrator, m_NormalizationCoefficient);
                m_IncomingSolar.push_back(iTotalSolar.sum(minLambda, maxLambda));
            }

            // Produce local results matrices for each side and property
            std::map<std::pair<Side, PropertySimple>, SquareMatrix> aResults;

            for(Side aSide : EnumSide())
            {
                // It is important to take a copy of aTotalA because it will be used to
                // multiply and integrate later and local values will change
                CMatrixSeries aTotalA = *m_Layer.getTotalA(aSide);
                aTotalA.mMult(*m_IncomingSpectra);
                aTotalA.integrate(m_Integrator, m_NormalizationCoefficient);
                m_Abs[aSide] = aTotalA.getSums(minLambda, maxLambda, m_IncomingSolar);
                for(PropertySimple aProprerty : EnumPropertySimple())
                {
                    // Same as for aTotalA. Copy need to be taken because of multiplication
                    // and integration
                    CMatrixSeries aTot = *m_Layer.getTotal(aSide, aProprerty);
                    aTot.mMult(*m_IncomingSpectra);
                    aTot.integrate(m_Integrator, m_NormalizationCoefficient);
                    aResults[std::make_pair(aSide, aProprerty)] =
                      aTot.getSquaredMatrixSums(minLambda, maxLambda, m_IncomingSolar);
                }

                // Update result matrices
                m_Results->setResultMatrices(aResults.at(std::make_pair(aSide, PropertySimple::T)),
                                             aResults.at(std::make_pair(aSide, PropertySimple::R)),
                                             aSide);
            }

            // calculate hemispherical absorptances
            for(Side aSide : EnumSide())
            {
                calcHemisphericalAbs(aSide);
            }

            m_MinLambdaCalculated = minLambda;
            m_MaxLambdaCalculated = maxLambda;
            m_Calculated = true;
        }
    }

    void CMultiPaneBSDF::calcHemisphericalAbs(const Side t_Side)
    {
        using ConstantsData::WCE_PI;
        const size_t numOfLayers = m_Abs[t_Side].size();
        std::vector<double> aLambdas = m_Results->lambdaVector();
        for(size_t layNum = 0; layNum < numOfLayers; ++layNum)
        {
            std::vector<double> aAbs = m_Abs[t_Side][layNum];
            assert(aAbs.size() == aLambdas.size());
            std::vector<double> mult(aLambdas.size());
            std::transform(aLambdas.begin(),
                           aLambdas.end(),
                           aAbs.begin(),
                           mult.begin(),
                           std::multiplies<double>());
            double sum = std::accumulate(mult.begin(), mult.end(), 0.0) / WCE_PI;
            m_AbsHem[t_Side]->push_back(sum);
        }
    }

    std::vector<double> & CMultiPaneBSDF::Abs(const double minLambda,
                                              const double maxLambda,
                                              const Side t_Side,
                                              const size_t Index)
    {
        calculate(minLambda, maxLambda);
        return m_Abs.at(t_Side)[Index - 1];
    }

    std::vector<double> CMultiPaneBSDF::DirHem(const double minLambda,
                                               const double maxLambda,
                                               const Side t_Side,
                                               const PropertySimple t_Property)
    {
        calculate(minLambda, maxLambda);
        return m_Results->DirHem(t_Side, t_Property);
    }

    double CMultiPaneBSDF::DirHem(const double minLambda,
                                  const double maxLambda,
                                  const Side t_Side,
                                  const PropertySimple t_Property,
                                  const double t_Theta,
                                  const double t_Phi)
    {
        const auto aIndex = m_Results->getNearestBeamIndex(t_Theta, t_Phi);
        return DirHem(minLambda, maxLambda, t_Side, t_Property)[aIndex];
    }

    double CMultiPaneBSDF::DirHem(const double minLambda,
                                  const double maxLambda,
                                  const Side t_Side,
                                  const PropertySimple t_Property,
                                  const size_t Index)
    {
        return DirHem(minLambda, maxLambda, t_Side, t_Property)[Index];
    }

    double CMultiPaneBSDF::Abs(const double minLambda,
                               const double maxLambda,
                               const Side t_Side,
                               const size_t layerIndex,
                               const double t_Theta,
                               const double t_Phi)
    {
        auto aIndex = m_Results->getNearestBeamIndex(t_Theta, t_Phi);
        return Abs(minLambda, maxLambda, t_Side, layerIndex)[aIndex];
    }

    double CMultiPaneBSDF::Abs(const double minLambda,
                               const double maxLambda,
                               const Side t_Side,
                               const size_t layerIndex,
                               const size_t beamIndex)
    {
        return Abs(minLambda, maxLambda, t_Side, layerIndex)[beamIndex];
    }

    double CMultiPaneBSDF::DiffDiff(const double minLambda,
                                    const double maxLambda,
                                    const Side t_Side,
                                    const PropertySimple t_Property)
    {
        calculate(minLambda, maxLambda);
        return m_Results->DiffDiff(t_Side, t_Property);
    }

    double CMultiPaneBSDF::AbsDiff(const double minLambda,
                                   const double maxLambda,
                                   const Side t_Side,
                                   const size_t t_LayerIndex)
    {
        calculate(minLambda, maxLambda);
        return (*m_AbsHem[t_Side])[t_LayerIndex - 1];
    }

    double CMultiPaneBSDF::energy(const double minLambda,
                                  const double maxLambda,
                                  const Side t_Side,
                                  const PropertySimple t_Property,
                                  const double t_Theta,
                                  const double t_Phi)
    {
        calculate(minLambda, maxLambda);
        const auto aIndex = m_Results->getNearestBeamIndex(t_Theta, t_Phi);
        const auto solarRadiation = m_IncomingSolar[aIndex];
        const auto dirHem = DirHem(minLambda, maxLambda, t_Side, t_Property)[aIndex];
        return dirHem * solarRadiation;
    }

    double CMultiPaneBSDF::energyAbs(const double minLambda,
                                     const double maxLambda,
                                     const Side t_Side,
                                     const size_t Index,
                                     const double t_Theta,
                                     const double t_Phi)
    {
        calculate(minLambda, maxLambda);
        auto aIndex = m_Results->getNearestBeamIndex(t_Theta, t_Phi);
        double solarRadiation = m_IncomingSolar[aIndex];
        double abs = Abs(minLambda, maxLambda, t_Side, Index)[aIndex];
        return abs * solarRadiation;
    }

    void CMultiPaneBSDF::setIntegrationType(FenestrationCommon::IntegrationType t_type,
                                            double normalizationCoefficient)
    {
        m_NormalizationCoefficient = normalizationCoefficient;
        m_Integrator = t_type;
    }

    void CMultiPaneBSDF::addLayer(const std::shared_ptr<SingleLayerOptics::CBSDFLayer> & t_Layer)
    {
        m_Layer.addLayer(t_Layer);
        m_Layer.setSolarRadiation(m_SolarRadiationInit);
    }

    std::unique_ptr<CMultiPaneBSDF>
      CMultiPaneBSDF::create(const std::shared_ptr<SingleLayerOptics::CBSDFLayer> & t_Layer,
                             const FenestrationCommon::CSeries & t_SolarRadiation,
                             const std::vector<double> & t_CommonWavelengths)
    {
        // make_shared will not work from private function so it needs to be created this way
        return std::unique_ptr<CMultiPaneBSDF>(
          new CMultiPaneBSDF({t_Layer}, t_SolarRadiation, t_CommonWavelengths));
    }

    std::unique_ptr<CMultiPaneBSDF>
      CMultiPaneBSDF::create(const std::shared_ptr<SingleLayerOptics::CBSDFLayer> & t_Layer,
                             const FenestrationCommon::CSeries & t_SolarRadiation,
                             const FenestrationCommon::CSeries & t_DetectorData,
                             const std::vector<double> & t_CommonWavelengths)
    {
        return std::unique_ptr<CMultiPaneBSDF>(
          new CMultiPaneBSDF({t_Layer}, t_SolarRadiation, t_DetectorData, t_CommonWavelengths));
    }

    std::unique_ptr<CMultiPaneBSDF> CMultiPaneBSDF::create(
      const std::vector<std::shared_ptr<SingleLayerOptics::CBSDFLayer>> & t_Layers,
      const FenestrationCommon::CSeries & t_SolarRadiation,
      const std::vector<double> & t_CommonWavelengths)
    {
        return std::unique_ptr<CMultiPaneBSDF>(
          new CMultiPaneBSDF(t_Layers, t_SolarRadiation, t_CommonWavelengths));
    }

    std::unique_ptr<CMultiPaneBSDF> CMultiPaneBSDF::create(
      const std::vector<std::shared_ptr<SingleLayerOptics::CBSDFLayer>> & t_Layers,
      const FenestrationCommon::CSeries & t_SolarRadiation,
      const FenestrationCommon::CSeries & t_DetectorData,
      const std::vector<double> & t_CommonWavelengths)
    {
        return std::unique_ptr<CMultiPaneBSDF>(
          new CMultiPaneBSDF(t_Layers, t_SolarRadiation, t_DetectorData, t_CommonWavelengths));
    }

    std::unique_ptr<CMultiPaneBSDF>
      CMultiPaneBSDF::create(const std::shared_ptr<SingleLayerOptics::CBSDFLayer> & t_Layer,
                             const FenestrationCommon::CSeries & t_SolarRadiation)
    {
        // make_shared will not work from private function so it needs to be created this way
        return std::unique_ptr<CMultiPaneBSDF>(new CMultiPaneBSDF({t_Layer}, t_SolarRadiation));
    }

    std::unique_ptr<CMultiPaneBSDF>
      CMultiPaneBSDF::create(const std::shared_ptr<SingleLayerOptics::CBSDFLayer> & t_Layer,
                             const FenestrationCommon::CSeries & t_SolarRadiation,
                             const FenestrationCommon::CSeries & t_DetectorData)
    {
        // make_shared will not work from private function so it needs to be created this way
        return std::unique_ptr<CMultiPaneBSDF>(
          new CMultiPaneBSDF({t_Layer}, t_SolarRadiation, t_DetectorData));
    }

    std::unique_ptr<CMultiPaneBSDF> CMultiPaneBSDF::create(
      const std::vector<std::shared_ptr<SingleLayerOptics::CBSDFLayer>> & t_Layers,
      const FenestrationCommon::CSeries & t_SolarRadiation)
    {
        return std::unique_ptr<CMultiPaneBSDF>(new CMultiPaneBSDF(t_Layers, t_SolarRadiation));
    }

    std::unique_ptr<CMultiPaneBSDF> CMultiPaneBSDF::create(
      const std::vector<std::shared_ptr<SingleLayerOptics::CBSDFLayer>> & t_Layers,
      const FenestrationCommon::CSeries & t_SolarRadiation,
      const FenestrationCommon::CSeries & t_DetectorData)
    {
        return std::unique_ptr<CMultiPaneBSDF>(
          new CMultiPaneBSDF(t_Layers, t_SolarRadiation, t_DetectorData));
    }
    double CMultiPaneBSDF::getPropertySimple(const double minLambda,
                                             const double maxLambda,
                                             const FenestrationCommon::PropertySimple t_Property,
                                             const FenestrationCommon::Side t_Side,
                                             const FenestrationCommon::Scattering t_Scattering,
                                             const double t_Theta,
                                             const double t_Phi)
    {
        double result{0};
        switch(t_Scattering)
        {
            case Scattering::DirectDirect:
                result = DirDir(minLambda, maxLambda, t_Side, t_Property, t_Theta, t_Phi);
                break;
            case Scattering::DirectDiffuse:
                result = DirHem(minLambda, maxLambda, t_Side, t_Property, t_Theta, t_Phi)
                         - DirDir(minLambda, maxLambda, t_Side, t_Property, t_Theta, t_Phi);
                break;
            case Scattering::DirectHemispherical:
                result = DirHem(minLambda, maxLambda, t_Side, t_Property, t_Theta, t_Phi);
                break;
            case Scattering::DiffuseDiffuse:
                result = DiffDiff(minLambda, maxLambda, t_Side, t_Property);
                break;
        }
        return result;
    }
    std::vector<double> CMultiPaneBSDF::getWavelengths() const
    {
        //return std::vector<double>();
		return m_Layer.getCommonWavelengths();
    }
    double CMultiPaneBSDF::getMinLambda() const
    {
        //return 0;
		return m_Layer.getMinLambda();
    }
    double CMultiPaneBSDF::getMaxLambda() const
    {
        //return 0;
		return m_Layer.getMaxLambda();
    }
    std::vector<double>
      CMultiPaneBSDF::getAbsorptanceLayers(const double minLambda,
                                           const double maxLambda,
                                           const FenestrationCommon::Side side,
                                           const FenestrationCommon::ScatteringSimple scattering,
                                           const double theta,
                                           const double phi)
    {
        std::vector<double> abs;
        size_t absSize{m_Abs.at(Side::Front).size()};
        for(size_t i = 1u; i <= absSize; ++i)
        {
            switch(scattering)
            {
                case ScatteringSimple::Direct:
                    abs.push_back(Abs(minLambda, maxLambda, side, i, theta, phi));
                    break;
                case ScatteringSimple::Diffuse:
                    abs.push_back(AbsDiff(minLambda, maxLambda, side, i));
                    break;
            }
        }
        return abs;
    }
    double CMultiPaneBSDF::DirDiff(double minLambda,
                                   double maxLambda,
                                   FenestrationCommon::Side t_Side,
                                   FenestrationCommon::PropertySimple t_Property,
                                   double t_Theta,
                                   double t_Phi)
    {
        return DirHem(minLambda, maxLambda, t_Side, t_Property, t_Theta, t_Phi)
               - DirDir(minLambda, maxLambda, t_Side, t_Property, t_Theta, t_Phi);
    }

}   // namespace MultiLayerOptics
