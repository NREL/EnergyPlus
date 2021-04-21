#include <sstream>

#include "EquivalentLayerSingleComponentMW.hpp"
#include "WCECommon.hpp"
#include "EquivalentLayerSingleComponent.hpp"

using namespace FenestrationCommon;

namespace MultiLayerOptics
{
    ///////////////////////////////////////////////////////////////////////////
    ///   CSurfaceSeries
    ///////////////////////////////////////////////////////////////////////////

    CSurfaceSeries::CSurfaceSeries(const CSeries &t_T,
                                   const CSeries &t_R)
    {
        m_Properties[Property::T] = t_T;
        m_Properties[Property::R] = t_R;
        size_t size = t_T.size();
        CSeries aAbs;
        for(size_t i = 0; i < size; ++i)
        {
            double wl = t_T[i].x();
            double t = t_T[i].value();
            double r = t_R[i].value();
            double value = 1 - t - r;
            if(value > (1 + ConstantsData::floatErrorTolerance)
               || value < -ConstantsData::floatErrorTolerance)
            {
                std::stringstream err_msg;
                err_msg << "Absorptance value for provided series is out of range.\n"
                        << "Wavelength: " << wl << "\nTransmittance: " << t
                        << "\nReflectance: " << r;
                throw std::runtime_error(err_msg.str());
            }
            aAbs.addProperty(wl, value);
        }
        m_Properties[Property::Abs] = aAbs;
    }

    CSeries CSurfaceSeries::getProperties(const Property t_Property) const
    {
        return m_Properties.at(t_Property);
    }

    ///////////////////////////////////////////////////////////////////////////
    ///   CLayerSeries
    ///////////////////////////////////////////////////////////////////////////

    CLayerSeries::CLayerSeries(const CSeries &t_Tf,
                               const CSeries &t_Rf,
                               const CSeries &t_Tb,
                               const CSeries &t_Rb)
    {
        m_Surfaces[Side::Front] = std::make_shared<CSurfaceSeries>(t_Tf, t_Rf);
        m_Surfaces[Side::Back] = std::make_shared<CSurfaceSeries>(t_Tb, t_Rb);
    }

    CSeries CLayerSeries::getProperties(const Side t_Side,
                                        const Property t_Property) const
    {
        return m_Surfaces.at(t_Side)->getProperties(t_Property);
    }

    ///////////////////////////////////////////////////////////////////////////
    ///   CEquivalentLayerSingleComponentMW
    ///////////////////////////////////////////////////////////////////////////

    CEquivalentLayerSingleComponentMW::CEquivalentLayerSingleComponentMW(
            const CSeries &t_Tf,
            const CSeries &t_Tb,
            const CSeries &t_Rf,
            const CSeries &t_Rb)
    {
        m_Layer = std::make_shared<CLayerSeries>(t_Tf, t_Rf, t_Tb, t_Rb);

        size_t size = t_Tf.size();
        for(size_t i = 0; i < size; ++i)
        {
            std::shared_ptr<CEquivalentLayerSingleComponent> aLayer =
              std::make_shared<CEquivalentLayerSingleComponent>(
                t_Tf[i].value(), t_Rf[i].value(), t_Tb[i].value(), t_Rb[i].value());
            m_EqLayerBySeries.push_back(aLayer);
        }
    }

    void CEquivalentLayerSingleComponentMW::addLayer(const CSeries &t_Tf,
                                                     const CSeries &t_Tb,
                                                     const CSeries &t_Rf,
                                                     const CSeries &t_Rb)
    {
        size_t size = t_Tf.size();

        for(size_t i = 0; i < size; ++i)
        {
            std::shared_ptr<CEquivalentLayerSingleComponent> aLayer = m_EqLayerBySeries[i];
            aLayer->addLayer(
              t_Tf[i].value(), t_Rf[i].value(), t_Tb[i].value(), t_Rb[i].value());
        }

        CSeries tTotf;
        CSeries tTotb;
        CSeries tRfTot;
        CSeries tRbTot;

        for(size_t i = 0; i < size; ++i)
        {
            double wl = t_Tf[i].x();

            double Tf = m_EqLayerBySeries[i]->getProperty(Property::T, Side::Front);
            tTotf.addProperty(wl, Tf);

            double Rf = m_EqLayerBySeries[i]->getProperty(Property::R, Side::Front);
            tRfTot.addProperty(wl, Rf);

            double Tb = m_EqLayerBySeries[i]->getProperty(Property::T, Side::Back);
            tTotb.addProperty(wl, Tb);

            double Rb = m_EqLayerBySeries[i]->getProperty(Property::R, Side::Back);
            tRbTot.addProperty(wl, Rb);
        }

        m_Layer = std::make_shared<CLayerSeries>(tTotf, tRfTot, tTotb, tRbTot);
    }

    CSeries
      CEquivalentLayerSingleComponentMW::getProperties(const Property t_Property,
                                                       const Side t_Side) const
    {
        return m_Layer->getProperties(t_Side, t_Property);
    }

}   // namespace MultiLayerOptics
