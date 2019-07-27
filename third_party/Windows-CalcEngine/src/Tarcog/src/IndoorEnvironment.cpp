
#include <cmath>
#include <stdexcept>
#include <cassert>

#include "IndoorEnvironment.hpp"

#include "WCEGases.hpp"
#include "Surface.hpp"
#include "WCECommon.hpp"


using namespace Gases;
using namespace FenestrationCommon;

namespace Tarcog {

	// Keep airspeed to zero and airdirection to default windward
	CIndoorEnvironment::CIndoorEnvironment( double const t_AirTemperature, double const t_Pressure ) :
		CEnvironment( t_Pressure, 0, AirHorizontalDirection::Windward ) {

		m_RoomRadiationTemperature = t_AirTemperature; // Radiation temperature is by default air
		m_Surface[ Side::Back ] = std::make_shared< CSurface >( m_Emissivity, 0 );
		m_Surface.at( Side::Back )->setTemperature( t_AirTemperature );
	}

	CIndoorEnvironment::CIndoorEnvironment( CIndoorEnvironment const& t_Indoor ) :
			CState( t_Indoor ), CEnvironment( t_Indoor ) {
		operator=( t_Indoor );
	}

	CIndoorEnvironment & CIndoorEnvironment::operator=( CIndoorEnvironment const & t_Environment ) {
		this->CState::operator=( t_Environment );
		this->CEnvironment::operator=( t_Environment );
		m_RoomRadiationTemperature = t_Environment.m_RoomRadiationTemperature;

		return *this;
	}

	void CIndoorEnvironment::connectToIGULayer( std::shared_ptr< CBaseLayer > const& t_IGULayer ) {
		t_IGULayer->connectToBackSide( shared_from_this() );
	}

	void CIndoorEnvironment::setRoomRadiationTemperature( double const t_RadiationTemperature ) {
		m_RoomRadiationTemperature = t_RadiationTemperature;
		resetCalculated();
	}

	std::shared_ptr< CBaseLayer > CIndoorEnvironment::clone() const {
		return cloneEnvironment();
	}

	std::shared_ptr< CEnvironment > CIndoorEnvironment::cloneEnvironment() const {
		return std::make_shared< CIndoorEnvironment >( *this );
	}

	double CIndoorEnvironment::getGasTemperature() {
		assert( m_Surface.at( Side::Back ) != nullptr );
		return m_Surface.at( Side::Back )->getTemperature();
	}

	double CIndoorEnvironment::calculateIRFromVariables() {
		using ConstantsData::STEFANBOLTZMANN;
		return STEFANBOLTZMANN * m_Emissivity * pow( m_RoomRadiationTemperature, 4 );
	}

	void CIndoorEnvironment::calculateConvectionOrConductionFlow() {
		// CEnvironment::calculateConvectionOrConductionFlow();
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
			throw std::runtime_error( "Incorrect definition for convection model (Indoor environment)." );
		}
		}
	}

	void CIndoorEnvironment::calculateHc() {
		if ( m_AirSpeed > 0 ) {
			m_ConductiveConvectiveCoeff = 4 + 4 * m_AirSpeed;
		}
		else {
			using ConstantsData::GRAVITYCONSTANT;
			using ConstantsData::PI;

			assert( m_Surface.at( Side::Front ) != nullptr );
			assert( m_Surface.at( Side::Back ) != nullptr );

			auto tiltRadians = m_Tilt * PI / 180;
			auto tMean = getGasTemperature() + 0.25 * ( m_Surface.at( Side::Front )->getTemperature() - getGasTemperature() );
			if ( tMean < 0 ) tMean = 0.1;
			auto deltaTemp = std::abs( m_Surface.at( Side::Front )->getTemperature() - getGasTemperature() );
			m_Gas->setTemperatureAndPressure( tMean, m_Pressure );
			auto aProperties = m_Gas->getGasProperties();
			auto gr = GRAVITYCONSTANT * pow( m_Height, 3 ) * deltaTemp * pow( aProperties->m_Density, 2 ) /
				( tMean * pow( aProperties->m_Viscosity, 2 ) );
			auto RaCrit = 2.5e5 * pow( exp( 0.72 * m_Tilt ) / sin( tiltRadians ), 0.2 );
			auto RaL = gr * aProperties->m_PrandlNumber;
			auto Gnui = 0.0;
			if ( ( 0.0 <= m_Tilt ) && ( m_Tilt < 15.0 ) ) {
				Gnui = 0.13 * pow( RaL, 1 / 3.0 );
			}
			else if ( ( 15.0 <= m_Tilt ) && ( m_Tilt <= 90.0 ) ) {
				if ( RaL <= RaCrit ) {
					Gnui = 0.56 * pow( RaL * sin( tiltRadians ), 0.25 );
				}
				else {
					Gnui = 0.13 * pow( RaL, 1 / 3.0 ) - pow( RaCrit, 1 / 3.0 ) + 0.56 * pow( RaCrit * sin( tiltRadians ), 0.25 );
				}
			}
			else if ( ( 90.0 < m_Tilt ) && ( m_Tilt <= 179.0 ) ) {
				Gnui = 0.56 * pow( RaL * sin( tiltRadians ), 0.25 );
			}
			else if ( ( 179.0 < m_Tilt ) && ( m_Tilt <= 180.0 ) ) {
				Gnui = 0.58 * pow( RaL, 1 / 3.0 );
			}
			m_ConductiveConvectiveCoeff = Gnui * aProperties->m_ThermalConductivity / m_Height;
		}
	}

	double CIndoorEnvironment::getHr() {
		assert( m_Surface.at( Side::Front ) != nullptr );
		return getRadiationFlow() / ( getRadiationTemperature() - m_Surface.at( Side::Front )->getTemperature() );
	}

	void CIndoorEnvironment::setIRFromEnvironment( double const t_IR ) {
		assert( m_Surface.at( Side::Back ) != nullptr );
		m_Surface.at( Side::Back )->setJ( t_IR );
	}

	double CIndoorEnvironment::getIRFromEnvironment() const {
		assert( m_Surface.at( Side::Back ) != nullptr );
		return m_Surface.at( Side::Back )->J();
	}

	double CIndoorEnvironment::getRadiationTemperature() const {
		return m_RoomRadiationTemperature;
	}
}
