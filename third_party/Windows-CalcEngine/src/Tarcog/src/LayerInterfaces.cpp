#include <cassert>
#include <stdexcept>

#include "LayerInterfaces.hpp"
#include "Surface.hpp"
#include "TarcogConstants.hpp"
#include "WCEGases.hpp"
#include "WCECommon.hpp"


using FenestrationCommon::Side;

namespace Tarcog
{
    namespace ISO15099
    {
        //////////////////////////////////////////////////////////////////////////
        ///      CLayerGeometry
        //////////////////////////////////////////////////////////////////////////

        CLayerGeometry::CLayerGeometry() :
            m_Width(TarcogConstants::DEFAULT_WINDOW_WIDTH),
            m_Height(TarcogConstants::DEFAULT_WINDOW_HEIGHT),
            m_Tilt(TarcogConstants::DEFAULT_TILT)
        {}

        void CLayerGeometry::setWidth(double const t_Width)
        {
            m_Width = t_Width;
            resetCalculated();
        }

        void CLayerGeometry::setHeight(double const t_Height)
        {
            m_Height = t_Height;
            resetCalculated();
        }

        void CLayerGeometry::setTilt(double const t_Tilt)
        {
            m_Tilt = t_Tilt;
            resetCalculated();
        }

        //////////////////////////////////////////////////////////////////////////
        ///      CLayerHeatFlow
        //////////////////////////////////////////////////////////////////////////

        CLayerHeatFlow::CLayerHeatFlow() : m_ConductiveConvectiveCoeff(0), m_LayerGainFlow(0)
        {
            m_Surface[Side::Front] = nullptr;
            m_Surface[Side::Back] = nullptr;
        }

        CLayerHeatFlow::CLayerHeatFlow(CLayerHeatFlow const & t_Layer) : CState(t_Layer)
        {
            operator=(t_Layer);
        }

        CLayerHeatFlow & CLayerHeatFlow::operator=(CLayerHeatFlow const & t_Layer)
        {
            this->CState::operator=(t_Layer);
            m_ConductiveConvectiveCoeff = t_Layer.m_ConductiveConvectiveCoeff;
            m_LayerGainFlow = t_Layer.m_LayerGainFlow;
            for(auto aSide : FenestrationCommon::EnumSide())
            {
                const auto aSurface = t_Layer.m_Surface.at(aSide);
                if(aSurface != nullptr)
                {
                    m_Surface[aSide] = aSurface->clone();
                }
            }

            return *this;
        }

        double CLayerHeatFlow::getHeatFlow()
        {
            return getRadiationFlow() + getConvectionConductionFlow();
        }

        double CLayerHeatFlow::getGainFlow()
        {
            calculateLayerHeatFlow();
            return m_LayerGainFlow;
        }

        double CLayerHeatFlow::getConductionConvectionCoefficient()
        {
            calculateLayerHeatFlow();
            return m_ConductiveConvectiveCoeff;
        }

        double CLayerHeatFlow::getRadiationFlow()
        {
            calculateRadiationFlow();
            assert(m_Surface.at(Side::Front) != nullptr);
            assert(m_Surface.at(Side::Back) != nullptr);
            return m_Surface.at(Side::Back)->J() - m_Surface.at(Side::Front)->J();
        }

        double CLayerHeatFlow::getConvectionConductionFlow()
        {
            calculateLayerHeatFlow();
            assert(m_Surface.at(Side::Front) != nullptr);
            assert(m_Surface.at(Side::Back) != nullptr);
            return (m_Surface.at(Side::Back)->getTemperature()
                    - m_Surface.at(Side::Front)->getTemperature())
                   * m_ConductiveConvectiveCoeff;
        }

        void CLayerHeatFlow::calculateLayerHeatFlow()
        {
            if(!isCalculated())
            {
                calculateRadiationFlow();
                calculateConvectionOrConductionFlow();
            }
            setCalculated();
        }

        bool CLayerHeatFlow::areSurfacesInitalized() const
        {
            auto areInitialized = (m_Surface.size() == 2);
            if(areInitialized)
            {
                areInitialized =
                  m_Surface.at(Side::Front) != nullptr && m_Surface.at(Side::Back) != nullptr;
            }
            return areInitialized;
        }

        std::shared_ptr<ISurface> CLayerHeatFlow::getSurface(Side const t_Position) const
        {
            return m_Surface.at(t_Position);
        }

        void CLayerHeatFlow::setSurface(std::shared_ptr<ISurface> t_Surface, Side const t_Position)
        {
            m_Surface[t_Position] = t_Surface;
            if(m_Surface.size() == 2)
            {
                resetCalculated();
            }
        }


        //////////////////////////////////////////////////////////////////////////
        ///      CGasLayer
        //////////////////////////////////////////////////////////////////////////

        CGasLayer::CGasLayer() :
            m_Pressure(0),
            m_AirSpeed(0),
            m_AirVerticalDirection(AirVerticalDirection::None),
            m_AirHorizontalDirection(AirHorizontalDirection::None)
        {}

        CGasLayer::CGasLayer(double const t_Pressure) :
            m_Pressure(t_Pressure),
            m_AirSpeed(0),
            m_AirVerticalDirection(AirVerticalDirection::None),
            m_AirHorizontalDirection(AirHorizontalDirection::None)
        {}

        CGasLayer::CGasLayer(double const t_Pressure,
                             double const t_AirSpeed,
                             AirVerticalDirection const t_AirVerticalDirection) :
            m_Pressure(t_Pressure),
            m_AirSpeed(t_AirSpeed),
            m_AirVerticalDirection(t_AirVerticalDirection),
            m_AirHorizontalDirection(AirHorizontalDirection::None)
        {}

        CGasLayer::CGasLayer(double const t_Pressure,
                             double const t_AirSpeed,
                             AirHorizontalDirection const t_AirHorizontalDirection) :
            m_Pressure(t_Pressure),
            m_AirSpeed(t_AirSpeed),
            m_AirVerticalDirection(AirVerticalDirection::None),
            m_AirHorizontalDirection(t_AirHorizontalDirection)
        {}

        CGasLayer::CGasLayer(double const t_Pressure, const Gases::CGas & t_Gas) :
            m_Pressure(t_Pressure),
            m_AirSpeed(0),
            m_AirVerticalDirection(AirVerticalDirection::None),
            m_AirHorizontalDirection(AirHorizontalDirection::None),
            m_Gas(t_Gas)
        {}

        double CGasLayer::getPressure()
        {
            return m_Pressure;
        }

        void CGasLayer::initializeStateVariables()
        {
            m_Gas.setTemperatureAndPressure(getGasTemperature(), m_Pressure);
        }

    }   // namespace ISO15099

}   // namespace Tarcog
