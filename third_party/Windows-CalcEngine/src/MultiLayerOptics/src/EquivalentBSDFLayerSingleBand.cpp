#include "EquivalentBSDFLayerSingleBand.hpp"
#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"

using namespace FenestrationCommon;
using namespace SingleLayerOptics;

namespace MultiLayerOptics
{
    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //  CInterReflectance
    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    CInterReflectance::CInterReflectance(const SquareMatrix & t_Lambda,
                                         const SquareMatrix & t_Rb,
                                         const SquareMatrix & t_Rf)
    {
        const auto size = t_Lambda.size();
        const auto lRb = t_Lambda * t_Rb;
        const auto lRf = t_Lambda * t_Rf;
        m_InterRefl = lRb * lRf;
        SquareMatrix I(size);
        I.setIdentity();
        m_InterRefl = I - m_InterRefl;
        m_InterRefl = m_InterRefl.inverse();
    }

    SquareMatrix CInterReflectance::value() const
    {
        return m_InterRefl;
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //  CBSDFDoubleLayer
    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    CBSDFDoubleLayer::CBSDFDoubleLayer(const CBSDFIntegrator & t_FrontLayer,
                                       const CBSDFIntegrator & t_BackLayer)
    {
        const SquareMatrix aLambda = t_FrontLayer.lambdaMatrix();
        CInterReflectance InterRefl1 =
          CInterReflectance(aLambda,
                            t_FrontLayer.at(Side::Back, PropertySimple::R),
                            t_BackLayer.at(Side::Front, PropertySimple::R));

        CInterReflectance InterRefl2 =
          CInterReflectance(aLambda,
                            t_BackLayer.at(Side::Front, PropertySimple::R),
                            t_FrontLayer.at(Side::Back, PropertySimple::R));

        m_Tf = equivalentT(t_BackLayer.at(Side::Front, PropertySimple::T),
                           InterRefl1.value(),
                           aLambda,
                           t_FrontLayer.at(Side::Front, PropertySimple::T));
        m_Tb = equivalentT(t_FrontLayer.at(Side::Back, PropertySimple::T),
                           InterRefl2.value(),
                           aLambda,
                           t_BackLayer.at(Side::Back, PropertySimple::T));
        m_Rf = equivalentR(t_FrontLayer.at(Side::Front, PropertySimple::R),
                           t_FrontLayer.at(Side::Front, PropertySimple::T),
                           t_FrontLayer.at(Side::Back, PropertySimple::T),
                           t_BackLayer.at(Side::Front, PropertySimple::R),
                           InterRefl2.value(),
                           aLambda);
        m_Rb = equivalentR(t_BackLayer.at(Side::Back, PropertySimple::R),
                           t_BackLayer.at(Side::Back, PropertySimple::T),
                           t_BackLayer.at(Side::Front, PropertySimple::T),
                           t_FrontLayer.at(Side::Back, PropertySimple::R),
                           InterRefl1.value(),
                           aLambda);

        m_Results = std::make_shared<CBSDFIntegrator>(t_FrontLayer);
        m_Results->setResultMatrices(m_Tf, m_Rf, Side::Front);
        m_Results->setResultMatrices(m_Tb, m_Rb, Side::Back);
    }

    std::shared_ptr<CBSDFIntegrator> CBSDFDoubleLayer::value() const
    {
        return m_Results;
    }

    SquareMatrix CBSDFDoubleLayer::equivalentT(const SquareMatrix & t_Tf2,
                                               const SquareMatrix & t_InterRefl,
                                               const SquareMatrix & t_Lambda,
                                               const SquareMatrix & t_Tf1)
    {
        const auto TinterRefl = t_Tf2 * t_InterRefl;
        const auto lambdaTf1 = t_Lambda * t_Tf1;
        return TinterRefl * lambdaTf1;
    }

    SquareMatrix CBSDFDoubleLayer::equivalentR(const SquareMatrix & t_Rf1,
                                               const SquareMatrix & t_Tf1,
                                               const SquareMatrix & t_Tb1,
                                               const SquareMatrix & t_Rf2,
                                               const SquareMatrix & t_InterRefl,
                                               const SquareMatrix & t_Lambda)
    {
        auto TinterRefl = t_Tb1 * t_InterRefl;
        const auto lambdaRf2 = t_Lambda * t_Rf2;
        const auto lambdaTf1 = t_Lambda * t_Tf1;
        TinterRefl = TinterRefl * lambdaRf2;
        TinterRefl = TinterRefl * lambdaTf1;
        return t_Rf1 + TinterRefl;
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //  CEquivalentBSDFLayerSingleBand
    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    CEquivalentBSDFLayerSingleBand::CEquivalentBSDFLayerSingleBand(
      const std::shared_ptr<CBSDFIntegrator> & t_Layer) :
        m_PropertiesCalculated(false)
    {
        m_EquivalentLayer = std::make_shared<CBSDFIntegrator>(t_Layer);
        for(Side aSide : EnumSide())
        {
            m_A[aSide] = std::vector<std::vector<double>>();
        }
        m_Layers.push_back(t_Layer);
        m_Lambda = t_Layer->lambdaMatrix();
    }

    SquareMatrix CEquivalentBSDFLayerSingleBand::getMatrix(const Side t_Side,
                                                           const PropertySimple t_Property)
    {
        calcEquivalentProperties();
        return m_EquivalentLayer->getMatrix(t_Side, t_Property);
    }

    SquareMatrix CEquivalentBSDFLayerSingleBand::getProperty(const Side t_Side,
                                                             const PropertySimple t_Property)
    {
        return getMatrix(t_Side, t_Property);
    }

    std::vector<double> CEquivalentBSDFLayerSingleBand::getLayerAbsorptances(const size_t Index,
                                                                             Side t_Side)
    {
        calcEquivalentProperties();
        return m_A.at(t_Side)[Index - 1];
    }

    size_t CEquivalentBSDFLayerSingleBand::getNumberOfLayers() const
    {
        return m_Layers.size();
    }

    void CEquivalentBSDFLayerSingleBand::addLayer(const std::shared_ptr<CBSDFIntegrator> & t_Layer)
    {
        m_Layers.push_back(t_Layer);
        m_PropertiesCalculated = false;
        for(Side aSide : EnumSide())
        {
            m_A.at(aSide).clear();
        }
    }

    void CEquivalentBSDFLayerSingleBand::calcEquivalentProperties()
    {
        if(m_PropertiesCalculated)
        {
            return;
        }
        // Absorptance calculations need to observe every layer in isolation. For that purpose
        // code bellow will create m_Forward and m_Backward layers
        size_t size = m_Layers.size();
        m_EquivalentLayer = m_Layers[0];
        m_Forward.push_back(m_EquivalentLayer);
        for(size_t i = 1; i < size; ++i)
        {
            m_EquivalentLayer = CBSDFDoubleLayer(*m_EquivalentLayer, *m_Layers[i]).value();
            m_Forward.push_back(m_EquivalentLayer);
        }
        m_Backward.push_back(m_EquivalentLayer);

        std::shared_ptr<CBSDFIntegrator> bLayer = m_Layers[size - 1];
        for(size_t i = size - 1; i > 1; --i)
        {
            bLayer = CBSDFDoubleLayer(*m_Layers[i - 1], *bLayer).value();
            m_Backward.push_back(bLayer);
        }
        m_Backward.push_back(m_Layers[size - 1]);

        const size_t matrixSize = m_Lambda.size();
        std::vector<double> zeros(matrixSize, 0);

        std::vector<double> Ap1f;
        std::vector<double> Ap2f;
        std::vector<double> Ap1b;
        std::vector<double> Ap2b;

        for(size_t i = 0; i < size; i++)
        {
            if(i == size - 1)
            {
                Ap2f = zeros;
                Ap1b = m_Layers[i]->Abs(Side::Back);
            }
            else
            {
                CBSDFIntegrator & Layer1 = *m_Backward[i + 1];
                CBSDFIntegrator & Layer2 = *m_Forward[i];
                CInterReflectance InterRefl2 =
                  CInterReflectance(m_Lambda,
                                    Layer1.at(Side::Front, PropertySimple::R),
                                    Layer2.at(Side::Back, PropertySimple::R));
                const std::vector<double> Ab = m_Layers[i]->Abs(Side::Back);
                Ap1b =
                  absTerm1(Ab, InterRefl2.value(), Layer1.getMatrix(Side::Back, PropertySimple::T));
                Ap2f = absTerm2(Ab,
                                InterRefl2.value(),
                                Layer1.getMatrix(Side::Front, PropertySimple::R),
                                Layer2.getMatrix(Side::Front, PropertySimple::T));
            }

            if(i == 0)
            {
                Ap1f = m_Layers[i]->Abs(Side::Front);
                Ap2b = zeros;
            }
            else
            {
                CBSDFIntegrator & Layer1 = *m_Forward[i - 1];
                CBSDFIntegrator & Layer2 = *m_Backward[i];
                CInterReflectance InterRefl1 =
                  CInterReflectance(m_Lambda,
                                    Layer1.at(Side::Back, PropertySimple::R),
                                    Layer2.at(Side::Front, PropertySimple::R));
                std::vector<double> Af = m_Layers[i]->Abs(Side::Front);
                Ap1f = absTerm1(Af, InterRefl1.value(), Layer1.at(Side::Front, PropertySimple::T));
                Ap2b = absTerm2(Af,
                                InterRefl1.value(),
                                Layer1.at(Side::Back, PropertySimple::R),
                                Layer2.at(Side::Back, PropertySimple::T));
            }

            std::map<Side, std::vector<double>> aTotal;
            for(Side aSide : EnumSide())
            {
                aTotal[aSide] = std::vector<double>();
            }
            for(size_t j = 0; j < matrixSize; ++j)
            {
                aTotal.at(Side::Front).push_back(Ap1f[j] + Ap2f[j]);
                aTotal.at(Side::Back).push_back(Ap1b[j] + Ap2b[j]);
            }

            for(Side aSide : EnumSide())
            {
                m_A.at(aSide).push_back(aTotal.at(aSide));
            }
        }
        m_PropertiesCalculated = true;
    }

    std::vector<double>
      CEquivalentBSDFLayerSingleBand::absTerm1(const std::vector<double> & t_Alpha,
                                               const SquareMatrix & t_InterRefl,
                                               const SquareMatrix & t_T) const
    {
        auto part1 = t_Alpha * t_InterRefl;
        const auto part2 = m_Lambda * t_T;
        part1 = part1 * part2;
        return part1;
    }

    std::vector<double>
      CEquivalentBSDFLayerSingleBand::absTerm2(const std::vector<double> & t_Alpha,
                                               const SquareMatrix & t_InterRefl,
                                               const SquareMatrix & t_R,
                                               const SquareMatrix & t_T) const
    {
        auto part1 = t_Alpha * t_InterRefl;
        const auto part2 = m_Lambda * t_R;
        const auto part3 = m_Lambda * t_T;
        part1 = part1 * part2;
        part1 = part1 * part3;
        return part1;
    }

}   // namespace MultiLayerOptics
