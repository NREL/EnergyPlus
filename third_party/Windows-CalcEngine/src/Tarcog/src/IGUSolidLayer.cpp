
#include <cmath>
#include <stdexcept>
#include <cassert>

#include "IGUSolidLayer.hpp"
#include "BaseLayer.hpp"
#include "Surface.hpp"
#include "WCECommon.hpp"
#include "TarcogConstants.hpp"
#include "LayerInterfaces.hpp"


using FenestrationCommon::Side;

namespace Tarcog
{
    namespace ISO15099
    {
        CIGUSolidLayer::CIGUSolidLayer(
          double const t_Thickness,
          double const t_Conductivity,
          std::shared_ptr<Tarcog::ISO15099::ISurface> const & t_FrontSurface,
          std::shared_ptr<Tarcog::ISO15099::ISurface> const & t_BackSurface) :
            CBaseIGULayer(t_Thickness),
            m_Conductivity(t_Conductivity),
            m_SolarAbsorptance(0)
        {
            if(t_FrontSurface != nullptr && t_BackSurface != nullptr)
            {
                m_Surface[Side::Front] = t_FrontSurface;
                m_Surface[Side::Back] = t_BackSurface;
            }
            else
            {
                m_Surface[Side::Front] = std::make_shared<CSurface>();
                m_Surface[Side::Back] = std::make_shared<CSurface>();
            }
        }

        CIGUSolidLayer::CIGUSolidLayer(double const t_Thickness,
                                       double const t_Conductivity,
                                       double const t_FrontEmissivity,
                                       double const t_FrontIRTransmittance,
                                       double const t_BackEmissivity,
                                       double const t_BackIRTransmittance) :
            CBaseIGULayer(t_Thickness),
            m_Conductivity(t_Conductivity),
            m_SolarAbsorptance(0)
        {
            m_Surface[Side::Front] =
              std::make_shared<CSurface>(t_FrontEmissivity, t_FrontIRTransmittance);
            m_Surface[Side::Back] =
              std::make_shared<CSurface>(t_BackEmissivity, t_BackIRTransmittance);
        }

        void CIGUSolidLayer::connectToBackSide(std::shared_ptr<CBaseLayer> const & t_Layer)
        {
            CBaseLayer::connectToBackSide(t_Layer);
            t_Layer->setSurface(m_Surface.at(Side::Back), Side::Front);
        }

        double CIGUSolidLayer::getConductance() const
        {
            return m_Conductivity;
        }

        double CIGUSolidLayer::getSolarAbsorptance() const
        {
            return m_SolarAbsorptance;
        }

        void CIGUSolidLayer::calculateConvectionOrConductionFlow()
        {
            if(m_Thickness == 0)
            {
                throw std::runtime_error("Solid layer thickness is set to zero.");
            }

            m_ConductiveConvectiveCoeff = m_Conductivity / m_Thickness;
        }

        void CIGUSolidLayer::setLayerState(double const t_Tf,
                                           double const t_Tb,
                                           double const t_Jf,
                                           double const t_Jb)
        {
            setSurfaceState(t_Tf, t_Jf, Side::Front);
            setSurfaceState(t_Tb, t_Jb, Side::Back);
            if(m_NextLayer != nullptr)
            {
                m_NextLayer->resetCalculated();
            }
            if(m_PreviousLayer != nullptr)
            {
                m_PreviousLayer->resetCalculated();
            }
        }

        void CIGUSolidLayer::setSurfaceState(double const t_Temperature,
                                             double const t_J,
                                             Side const t_Position)
        {
            std::shared_ptr<ISurface> aSurface = m_Surface.at(t_Position);
            aSurface->setTemperature(t_Temperature);
            aSurface->setJ(t_J);

            resetCalculated();
        }

        void CIGUSolidLayer::setSolarRadiation(double const t_SolarRadiation)
        {
            m_LayerGainFlow = t_SolarRadiation * m_SolarAbsorptance;
            resetCalculated();
        }

        void CIGUSolidLayer::setSolarAbsorptance(double const t_SolarAbsorptance,
                                                 const double t_SolarRadiation)
        {
            m_SolarAbsorptance = t_SolarAbsorptance;
            m_LayerGainFlow = t_SolarRadiation * m_SolarAbsorptance;
            resetCalculated();
        }

        std::shared_ptr<CBaseLayer> CIGUSolidLayer::clone() const
        {
            return std::make_shared<CIGUSolidLayer>(*this);
        }

        bool CIGUSolidLayer::isDeflected() const
        {
            return false;
        }

        double CIGUSolidLayer::getRadiationFlow()
        {
            // Solid layers share surfaces, so actually asking for front surface of previous layer
            // will be actual incoming radiation to this surface layer. And vice versa for back
            // surface.
            const auto frontIncomingRadiation{
              getPreviousLayer()->getSurface(FenestrationCommon::Side::Front)->J()};
            const auto backIncomingRadiation{
              getNextLayer()->getSurface(FenestrationCommon::Side::Back)->J()};

            const auto frontSurface{m_Surface.at(Side::Front)};

            const auto tir{frontSurface->getTransmittance()};
            const auto radiationFlow{tir * (backIncomingRadiation - frontIncomingRadiation)};
            return radiationFlow;
        }
    }   // namespace ISO15099

}   // namespace Tarcog
