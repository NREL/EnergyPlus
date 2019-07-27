
#include <cmath>
#include <stdexcept>
#include <cassert>

#include "IGUSolidLayer.hpp"
#include "BaseLayer.hpp"
#include "Surface.hpp"
#include "WCECommon.hpp"
#include "TarcogConstants.hpp"
#include "LayerInterfaces.hpp"


using namespace FenestrationCommon;

namespace Tarcog {

	CIGUSolidLayer::CIGUSolidLayer( double const t_Thickness, double const t_Conductivity,
	                                std::shared_ptr< ISurface > const& t_FrontSurface, std::shared_ptr< ISurface > const& t_BackSurface )
		: CState(), CBaseIGULayer( t_Thickness ), m_Conductivity( t_Conductivity ),
		  m_SolarAbsorptance( 0 ) {
		if ( t_FrontSurface != nullptr && t_BackSurface != nullptr ) {
			m_Surface[ Side::Front ] = t_FrontSurface;
			m_Surface[ Side::Back ] = t_BackSurface;
		}
		else {
			m_Surface[ Side::Front ] = std::make_shared< CSurface >();
			m_Surface[ Side::Back ] = std::make_shared< CSurface >();
		}
	}

	CIGUSolidLayer::CIGUSolidLayer( double const t_Thickness, double const t_Conductivity,
	                                double const t_FrontEmissivity, double const t_FrontIRTransmittance,
	                                double const t_BackEmissivity, double const t_BackIRTransmittance ) : CState(),
	                                                                                                      CBaseIGULayer( t_Thickness ), m_Conductivity( t_Conductivity ), m_SolarAbsorptance( 0 ) {
		m_Surface[ Side::Front ] = std::make_shared< CSurface >( t_FrontEmissivity, t_FrontIRTransmittance );
		m_Surface[ Side::Back ] = std::make_shared< CSurface >( t_BackEmissivity, t_BackIRTransmittance );
	}

	CIGUSolidLayer::CIGUSolidLayer( CIGUSolidLayer const& t_Layer ) :
			CState( t_Layer ), CBaseIGULayer( t_Layer ) {
		operator=( t_Layer );
	}

	CIGUSolidLayer & CIGUSolidLayer::operator=( CIGUSolidLayer const & t_Layer ) {
		this->CState::operator=( t_Layer );
		this->CBaseIGULayer::operator=( t_Layer );
		m_Conductivity = t_Layer.m_Conductivity;
		m_SolarAbsorptance = t_Layer.m_SolarAbsorptance;

		return *this;
	}

	void CIGUSolidLayer::connectToBackSide( std::shared_ptr< CBaseLayer > const& t_Layer ) {
		CBaseLayer::connectToBackSide( t_Layer );
		t_Layer->setSurface( m_Surface.at( Side::Back ), Side::Front );
	}

	double CIGUSolidLayer::getConductivity() const {
		return m_Conductivity;
	}

	void CIGUSolidLayer::calculateConvectionOrConductionFlow() {
		if ( m_Thickness == 0 ) {
			throw std::runtime_error( "Solid layer thickness is set to zero." );
		}

		m_ConductiveConvectiveCoeff = m_Conductivity / m_Thickness;

	}

	void CIGUSolidLayer::setLayerState( double const t_Tf, double const t_Tb,
	                                    double const t_Jf, double const t_Jb ) {
		setSurfaceState( t_Tf, t_Jf, Side::Front );
		setSurfaceState( t_Tb, t_Jb, Side::Back );
		if ( m_NextLayer != nullptr ) {
			m_NextLayer->resetCalculated();
		}
		if ( m_PreviousLayer != nullptr ) {
			m_PreviousLayer->resetCalculated();
		}
	}

	void CIGUSolidLayer::setSurfaceState( double const t_Temperature, double const t_J,
	                                      Side const t_Position ) {
		std::shared_ptr< ISurface > aSurface = m_Surface.at( t_Position );
		aSurface->setTemperature( t_Temperature );
		aSurface->setJ( t_J );

		resetCalculated();
	}

	void CIGUSolidLayer::setSolarRadiation( double const t_SolarRadiation ) {
		m_LayerGainFlow = t_SolarRadiation * m_SolarAbsorptance;
		resetCalculated();
	}

	void CIGUSolidLayer::setSolarAbsorptance( double const t_SolarAbsorptance ) {
		m_SolarAbsorptance = t_SolarAbsorptance;
		resetCalculated();
	}

	std::shared_ptr< CBaseLayer > CIGUSolidLayer::clone() const {
		return std::make_shared< CIGUSolidLayer >( *this );
	}

}
