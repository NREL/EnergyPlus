#define _USE_MATH_DEFINES
#include <math.h>
#include <stdexcept>
#include <cassert>

#include "WCEGases.hpp"
#include "Surface.hpp"
#include "TarcogConstants.hpp"
#include "OutdoorEnvironment.hpp"
#include "WCECommon.hpp"


using namespace ConstantsData;
using namespace FenestrationCommon;

namespace Tarcog {

	using namespace TarcogConstants;

	COutdoorEnvironment::COutdoorEnvironment( double const t_AirTemperature, double const t_Pressure,
	                                          double const t_AirSpeed, double const t_DirectSolarRadiation, AirHorizontalDirection const t_AirDirection,
	                                          double const t_SkyTemperature, SkyModel const t_Model, double const t_FractionClearSky ) :
		CEnvironment( t_Pressure, t_AirSpeed, t_AirDirection ), m_Tsky( t_SkyTemperature ),
		m_FractionOfClearSky( t_FractionClearSky ), m_SkyModel( t_Model ) {
		m_Surface[ Side::Front ] = std::make_shared< CSurface >();
		m_Surface.at( Side::Front )->setTemperature( t_AirTemperature );
		m_DirectSolarRadiation = t_DirectSolarRadiation;
	}

	COutdoorEnvironment::COutdoorEnvironment( COutdoorEnvironment const& t_Outdoor ) :
		CState( t_Outdoor ), CEnvironment( t_Outdoor ) {
		operator=( t_Outdoor );
	}

	COutdoorEnvironment & COutdoorEnvironment::operator=( COutdoorEnvironment const & t_Outdoor ) {
		this->CState::operator=( t_Outdoor );
		this->CEnvironment::operator=( t_Outdoor );
		m_Tsky = t_Outdoor.m_Tsky;
		m_FractionOfClearSky = t_Outdoor.m_FractionOfClearSky;
		m_SkyModel = t_Outdoor.m_SkyModel;

		return *this;
	}

	double COutdoorEnvironment::calculateIRFromVariables() {
		auto aEmissivity = 0.0;
		switch ( m_SkyModel ) {
		case AllSpecified:
			aEmissivity = m_Emissivity * pow( m_Tsky, 4 ) / pow( getAirTemperature(), 4 );
			break;
		case TSkySpecified:
			aEmissivity = pow( m_Tsky, 4 ) / pow( getAirTemperature(), 4 );
			break;
		case Swinbank:
			aEmissivity = 5.31e-13 * pow( getAirTemperature(), 6 ) / ( STEFANBOLTZMANN * pow( getAirTemperature(), 4 ) );
			break;
		default:
			throw std::runtime_error( "Incorrect sky model specified." );
			break;
		}

		auto radiationTemperature = 0.0;
		if ( m_HCoefficientModel == BoundaryConditionsCoeffModel::HPrescribed ) {
			radiationTemperature = getAirTemperature();
		}
		else {
			auto fSky = ( 1 + cos( m_Tilt * M_PI / 180 ) ) / 2;
			auto fGround = 1 - fSky;
			auto eZero = fGround + ( 1 - m_FractionOfClearSky ) * fSky + fSky * m_FractionOfClearSky * aEmissivity;
			radiationTemperature = getAirTemperature() * pow( eZero, 0.25 );
		}

		return STEFANBOLTZMANN * pow( radiationTemperature, 4 );
	}

	void COutdoorEnvironment::connectToIGULayer( std::shared_ptr< CBaseLayer > const& t_IGULayer ) {
		this->connectToBackSide( t_IGULayer );
		m_Surface[ Side::Back ] = t_IGULayer->getSurface( Side::Front );
	}

	std::shared_ptr< CBaseLayer > COutdoorEnvironment::clone() const {
		return cloneEnvironment();
	}

	std::shared_ptr< CEnvironment > COutdoorEnvironment::cloneEnvironment() const {
		return std::make_shared< COutdoorEnvironment >( *this );
	}

	void COutdoorEnvironment::setSolarRadiation( double const t_SolarRadiation ) {
		m_DirectSolarRadiation = t_SolarRadiation;
	}

	double COutdoorEnvironment::getSolarRadiation() const {
		return m_DirectSolarRadiation;
	}

	double COutdoorEnvironment::getGasTemperature() {
		assert( m_Surface.at( Side::Front ) != nullptr );
		return m_Surface.at( Side::Front )->getTemperature();
	}

	void COutdoorEnvironment::calculateConvectionOrConductionFlow() {
		switch ( m_HCoefficientModel ) {
		case BoundaryConditionsCoeffModel::CalculateH: {
			calculateHc();
			break;
		}
		case BoundaryConditionsCoeffModel::HPrescribed: {
			auto hr = getHr();
			m_ConductiveConvectiveCoeff = m_HInput - hr;
			break;
		}
		case BoundaryConditionsCoeffModel::HcPrescribed: {
			m_ConductiveConvectiveCoeff = m_HInput;
			break;
		}
		default: {
			throw std::runtime_error( "Incorrect definition for convection model (Outdoor environment)." );
		}
		}
	}

	void COutdoorEnvironment::calculateHc() {
		m_ConductiveConvectiveCoeff = 4 + 4 * m_AirSpeed;
	}

	double COutdoorEnvironment::getHr() {
		assert( m_Surface.at( Side::Back ) != nullptr );
		assert( m_Surface.at( Side::Front ) != nullptr );
		return getRadiationFlow() / ( m_Surface.at( Side::Back )->getTemperature() - getRadiationTemperature() );
	}

	double COutdoorEnvironment::getRadiationTemperature() const {
		assert( m_Surface.at( Side::Front ) != nullptr );
		return pow( m_Surface.at( Side::Front )->J() / STEFANBOLTZMANN, 0.25 );
	}

	void COutdoorEnvironment::setIRFromEnvironment( double const t_IR ) {
		assert( m_Surface.at( Side::Front ) != nullptr );
		m_Surface.at( Side::Front )->setJ( t_IR );
	}

	double COutdoorEnvironment::getIRFromEnvironment() const {
		assert( m_Surface.at( Side::Front ) != nullptr );
		return m_Surface.at( Side::Front )->J();
	}

}
