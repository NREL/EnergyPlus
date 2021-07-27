#include <cassert>

#include "MultiLayerScattered.hpp"
#include "MultiLayerInterRef.hpp"
#include "EquivalentScatteringLayer.hpp"
#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"

using namespace SingleLayerOptics;
using namespace FenestrationCommon;

namespace MultiLayerOptics
{
    CMultiLayerScattered::CMultiLayerScattered(const double t_Tf_dir_dir,
                                               const double t_Rf_dir_dir,
                                               const double t_Tb_dir_dir,
                                               const double t_Rb_dir_dir,
                                               const double t_Tf_dir_dif,
                                               const double t_Rf_dir_dif,
                                               const double t_Tb_dir_dif,
                                               const double t_Rb_dir_dif,
                                               const double t_Tf_dif_dif,
                                               const double t_Rf_dif_dif,
                                               const double t_Tb_dif_dif,
                                               const double t_Rb_dif_dif) :
        m_Calculated(false),
        m_Theta(0),
        m_Phi(0)
    {
        CScatteringLayer aLayer(t_Tf_dir_dir,
                                t_Rf_dir_dir,
                                t_Tb_dir_dir,
                                t_Rb_dir_dir,
                                t_Tf_dir_dif,
                                t_Rf_dir_dif,
                                t_Tb_dir_dif,
                                t_Rb_dir_dif,
                                t_Tf_dif_dif,
                                t_Rf_dif_dif,
                                t_Tb_dif_dif,
                                t_Rb_dif_dif);

        initialize(aLayer);
    }

    CMultiLayerScattered::CMultiLayerScattered(const CScatteringLayer & t_Layer) :
        m_Calculated(false),
        m_Theta(0),
        m_Phi(0)
    {
        initialize(t_Layer);
    }

    void CMultiLayerScattered::addLayer(const double t_Tf_dir_dir,
                                        const double t_Rf_dir_dir,
                                        const double t_Tb_dir_dir,
                                        const double t_Rb_dir_dir,
                                        const double t_Tf_dir_dif,
                                        const double t_Rf_dir_dif,
                                        const double t_Tb_dir_dif,
                                        const double t_Rb_dir_dif,
                                        const double t_Tf_dif_dif,
                                        const double t_Rf_dif_dif,
                                        const double t_Tb_dif_dif,
                                        const double t_Rb_dif_dif,
                                        const Side t_Side)
    {
        CScatteringLayer aLayer(t_Tf_dir_dir,
                                t_Rf_dir_dir,
                                t_Tb_dir_dir,
                                t_Rb_dir_dir,
                                t_Tf_dir_dif,
                                t_Rf_dir_dif,
                                t_Tb_dir_dif,
                                t_Rb_dir_dif,
                                t_Tf_dif_dif,
                                t_Rf_dif_dif,
                                t_Tb_dif_dif,
                                t_Rb_dif_dif);

        addLayer(aLayer, t_Side);
    }

    void CMultiLayerScattered::addLayer(const CScatteringLayer & t_Layer, const Side t_Side)
    {
        switch(t_Side)
        {
            case Side::Front:
                m_Layers.insert(m_Layers.begin(), t_Layer);
                break;
            case Side::Back:
                m_Layers.push_back(t_Layer);
                break;
            default:
                assert("Incorrect side selected.");
                break;
        }
        m_Calculated = false;
    }

    void CMultiLayerScattered::setSourceData(CSeries & t_SourceData)
    {
        for(auto & layer : m_Layers)
        {
            layer.setSourceData(t_SourceData);
            m_Calculated = false;
        }
    }

    size_t CMultiLayerScattered::getNumOfLayers() const
    {
        return m_Layers.size();
    }

    double CMultiLayerScattered::getPropertySimple(const double,
                                                   const double,
                                                   const PropertySimple t_Property,
                                                   const Side t_Side,
                                                   const Scattering t_Scattering,
                                                   const double t_Theta,
                                                   const double t_Phi)
    {
        calculateState(t_Theta, t_Phi);
        return m_Layer->getPropertySimple(t_Property, t_Side, t_Scattering, t_Theta, t_Phi);
    }

    double CMultiLayerScattered::getAbsorptanceLayer(const size_t Index,
                                                     Side t_Side,
                                                     ScatteringSimple t_Scattering,
                                                     const double t_Theta,
                                                     const double t_Phi)
    {
        return getAbsorptanceLayer(0, 0, Index, t_Side, t_Scattering, t_Theta, t_Phi);
    }

    double
      CMultiLayerScattered::getAbsorptanceLayer(double,
                                                double,
                                                size_t Index,
                                                FenestrationCommon::Side t_Side,
                                                FenestrationCommon::ScatteringSimple t_Scattering,
                                                double t_Theta,
                                                double t_Phi)
    {
        calculateState(t_Theta, t_Phi);
        return m_InterRef->getAbsorptance(Index, t_Side, t_Scattering, t_Theta, t_Phi);
    }

    std::vector<double>
      CMultiLayerScattered::getAbsorptanceLayers(const double minLambda,
                                                 const double maxLambda,
                                                 FenestrationCommon::Side side,
                                                 FenestrationCommon::ScatteringSimple scattering,
                                                 const double theta,
                                                 const double phi)
    {
        std::vector<double> abs;
        for(size_t i = 0u; i < m_Layers.size(); ++i)
        {
            abs.push_back(
              getAbsorptanceLayer(minLambda, maxLambda, i, side, scattering, theta, phi));
        }
        return abs;
    }

    double CMultiLayerScattered::getAbsorptance(Side t_Side,
                                                ScatteringSimple t_Scattering,
                                                const double t_Theta,
                                                const double t_Phi)
    {
        calculateState(t_Theta, t_Phi);
        double aAbs = 0;
        for(size_t i = 0; i < m_InterRef->size(); ++i)
        {
            aAbs += m_InterRef->getAbsorptance(i + 1, t_Side, t_Scattering, t_Theta, t_Phi);
        }
        return aAbs;
    }

    void CMultiLayerScattered::initialize(const CScatteringLayer & t_Layer)
    {
        m_Layers.push_back(t_Layer);
    }

    void CMultiLayerScattered::calculateState(const double t_Theta, const double t_Phi)
    {
        if(!m_Calculated || (t_Theta != m_Theta) || (t_Phi != m_Phi))
        {
            m_Layer = std::make_shared<CEquivalentScatteringLayer>(m_Layers[0], t_Theta, t_Phi);
            m_InterRef = std::make_shared<CInterRef>(m_Layers[0], t_Theta, t_Phi);
            for(size_t i = 1; i < m_Layers.size(); ++i)
            {
                m_Layer->addLayer(m_Layers[i], Side::Back, t_Theta, t_Phi);
                m_InterRef->addLayer(m_Layers[i], Side::Back, t_Theta, t_Phi);
            }
            m_Calculated = true;
            m_Theta = t_Theta;
            m_Phi = t_Phi;
        }
    }

    std::vector<double> CMultiLayerScattered::getWavelengths() const
    {
        return m_Layers[0].getWavelengths();
    }

    double CMultiLayerScattered::getMinLambda() const
    {
        return m_Layers[0].getMinLambda();
    }

    double CMultiLayerScattered::getMaxLambda() const
    {
        return m_Layers[0].getMaxLambda();
    }

    std::unique_ptr<CMultiLayerScattered>
      CMultiLayerScattered::create(const SingleLayerOptics::CScatteringLayer & t_Layer)
    {
        return std::unique_ptr<CMultiLayerScattered>(new CMultiLayerScattered(t_Layer));
    }

    std::unique_ptr<CMultiLayerScattered>
      CMultiLayerScattered::create(const std::vector<SingleLayerOptics::CScatteringLayer> & layers)
    {
        return std::unique_ptr<CMultiLayerScattered>(new CMultiLayerScattered(layers));
    }

    CMultiLayerScattered::CMultiLayerScattered(
      const std::vector<SingleLayerOptics::CScatteringLayer> & layers)
    {
        for(const auto & layer : layers)
        {
            addLayer(layer);
        }
    }


}   // namespace MultiLayerOptics
