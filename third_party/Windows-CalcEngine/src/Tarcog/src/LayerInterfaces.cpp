#include <cassert>
#include <stdexcept>

#include "LayerInterfaces.hpp"
#include "Surface.hpp"
#include "TarcogConstants.hpp"
#include "WCEGases.hpp"
#include "WCECommon.hpp"


using namespace FenestrationCommon;
using namespace Gases;

namespace Tarcog {

	using namespace TarcogConstants;

	//////////////////////////////////////////////////////////////////////////
	//      CLayerGeometry
	//////////////////////////////////////////////////////////////////////////

	CLayerGeometry::CLayerGeometry() : m_Width( DEFAULT_WINDOW_WIDTH ), m_Height( DEFAULT_WINDOW_HEIGHT ),
	                                   m_Tilt( DEFAULT_TILT ) {

	}

	CLayerGeometry::CLayerGeometry( CLayerGeometry const& t_Layer ) :
		CState( t_Layer ) {
		m_Height = t_Layer.m_Height;
		m_Width = t_Layer.m_Width;
		m_Tilt = t_Layer.m_Tilt;
	}

	void CLayerGeometry::setWidth( double const t_Width ) {
		m_Width = t_Width;
		resetCalculated();
	}

	void CLayerGeometry::setHeight( double const t_Height ) {
		m_Height = t_Height;
		resetCalculated();
	}

	void CLayerGeometry::setTilt( double const t_Tilt ) {
		m_Tilt = t_Tilt;
		resetCalculated();
	}

	//////////////////////////////////////////////////////////////////////////
	//      CLayerHeatFlow
	//////////////////////////////////////////////////////////////////////////

	CLayerHeatFlow::CLayerHeatFlow() : m_ConductiveConvectiveCoeff( 0 ), m_LayerGainFlow( 0 ) {
		m_Surface[ Side::Front ] = nullptr;
		m_Surface[ Side::Back ] = nullptr;
	}

	CLayerHeatFlow::CLayerHeatFlow( CLayerHeatFlow const& t_Layer ) :
		CState( t_Layer ) {
		m_ConductiveConvectiveCoeff = t_Layer.m_ConductiveConvectiveCoeff;
		m_LayerGainFlow = t_Layer.m_LayerGainFlow;
		for ( auto aSide : EnumSide() ) {
			auto aSurface = t_Layer.m_Surface.at( aSide );
			if ( aSurface != nullptr ) {
				m_Surface[ aSide ] = aSurface->clone();
			}
		}
	}

	CLayerHeatFlow::CLayerHeatFlow( std::shared_ptr< ISurface > const& t_FrontSurface,
	                                std::shared_ptr< ISurface > const& t_BackSurface ) :
		m_ConductiveConvectiveCoeff( 0 ), m_LayerGainFlow( 0 ) {
		m_Surface[ Side::Front ] = t_FrontSurface;
		m_Surface[ Side::Back ] = t_BackSurface;
	}

	double CLayerHeatFlow::getHeatFlow() {
		return getRadiationFlow() + getConvectionConductionFlow();
	}

	double CLayerHeatFlow::getGainFlow() {
		calculateLayerHeatFlow();
		return m_LayerGainFlow;
	}

	double CLayerHeatFlow::getConductionConvectionCoefficient() {
		calculateLayerHeatFlow();
		return m_ConductiveConvectiveCoeff;
	}

	double CLayerHeatFlow::getRadiationFlow() {
		calculateRadiationFlow();
		assert( m_Surface.at( Side::Front ) != nullptr );
		assert( m_Surface.at( Side::Back ) != nullptr );
		return m_Surface.at( Side::Back )->J() - m_Surface.at( Side::Front )->J();
	}

	double CLayerHeatFlow::getConvectionConductionFlow() {
		calculateLayerHeatFlow();
		assert( m_Surface.at( Side::Front ) != nullptr );
		assert( m_Surface.at( Side::Back ) != nullptr );
		return ( m_Surface.at( Side::Back )->getTemperature() -
			m_Surface.at( Side::Front )->getTemperature() ) * m_ConductiveConvectiveCoeff;
	}

	void CLayerHeatFlow::calculateLayerHeatFlow() {
		if ( !isCalculated() ) {
			calculateRadiationFlow();
			calculateConvectionOrConductionFlow();
		}
		setCalculated();
	}

	bool CLayerHeatFlow::areSurfacesInitalized() const {
		auto areInitialized = ( m_Surface.size() == 2 );
		if ( areInitialized ) {
			areInitialized = m_Surface.at( Side::Front ) != nullptr && m_Surface.at( Side::Back ) != nullptr;
		}
		return areInitialized;
	}

	std::shared_ptr< ISurface > CLayerHeatFlow::getSurface( Side const t_Position ) const {
		return m_Surface.at( t_Position );
	}

	void CLayerHeatFlow::setSurface( std::shared_ptr< ISurface > t_Surface,
	                                 Side const t_Position ) {
		m_Surface[ t_Position ] = t_Surface;
		if ( m_Surface.size() == 2 ) {
			resetCalculated();
		}
	}


	//////////////////////////////////////////////////////////////////////////
	//      CGasLayer
	//////////////////////////////////////////////////////////////////////////

	CGasLayer::CGasLayer() : m_Pressure( 0 ), m_AirSpeed( 0 ),
	                         m_AirVerticalDirection( AirVerticalDirection::None ),
	                         m_AirHorizontalDirection( AirHorizontalDirection::None ) {
		onCreate();
	}

	CGasLayer::CGasLayer( double const t_Pressure ) : m_Pressure( t_Pressure ), m_AirSpeed( 0 ),
	                                                  m_AirVerticalDirection( AirVerticalDirection::None ),
	                                                  m_AirHorizontalDirection( AirHorizontalDirection::None ) {
		onCreate();
	}

	CGasLayer::CGasLayer( double const t_Pressure, double const t_AirSpeed,
	                      AirVerticalDirection const t_AirVerticalDirection ) : m_Pressure( t_Pressure ),
	                                                                            m_AirSpeed( t_AirSpeed ), m_AirVerticalDirection( t_AirVerticalDirection ),
	                                                                            m_AirHorizontalDirection( AirHorizontalDirection::None ) {
		onCreate();
	}

	CGasLayer::CGasLayer( double const t_Pressure, double const t_AirSpeed,
	                      AirHorizontalDirection const t_AirHorizontalDirection ) : m_Pressure( t_Pressure ),
	                                                                                m_AirSpeed( t_AirSpeed ), m_AirVerticalDirection( AirVerticalDirection::None ),
	                                                                                m_AirHorizontalDirection( t_AirHorizontalDirection ) {
		onCreate();
	}

	CGasLayer::CGasLayer( double const t_Pressure, std::shared_ptr< CGas > const& t_Gas ) :
		m_Pressure( t_Pressure ), m_AirSpeed( 0 ),
		m_AirVerticalDirection( AirVerticalDirection::None ),
		m_AirHorizontalDirection( AirHorizontalDirection::None ) {
		m_Gas = t_Gas;
		onCreate();
	}

	CGasLayer::CGasLayer( CGasLayer const& t_Layer ) : CState( t_Layer ) {
		m_Pressure = t_Layer.m_Pressure;
		m_AirSpeed = t_Layer.m_AirSpeed;
		m_AirVerticalDirection = t_Layer.m_AirVerticalDirection;
		m_AirHorizontalDirection = t_Layer.m_AirHorizontalDirection;
		m_ForcedVentilation = t_Layer.m_ForcedVentilation;
		m_Gas = t_Layer.m_Gas;
	}

	double CGasLayer::getPressure() {
		return m_Pressure;
	}

	void CGasLayer::initializeStateVariables() {
		assert( m_Gas != nullptr );
		m_Gas->setTemperatureAndPressure( getGasTemperature(), m_Pressure );
	}

	void CGasLayer::onCreate() {

		// new gas will be created only if not passed from constructor.
		if ( m_Gas == nullptr ) {
			m_Gas = std::make_shared< Gases::CGas >();
		}
		m_ForcedVentilation = ForcedVentilation();
	}

}
