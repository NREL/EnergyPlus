#define _USE_MATH_DEFINES
#include <math.h>
#include <cassert>

#include "IGUVentilatedGapLayer.hpp"
#include "WCEGases.hpp"


using namespace Gases;

namespace Tarcog {

	CIGUVentilatedGapLayer::CIGUVentilatedGapLayer( std::shared_ptr< CIGUGapLayer > const& t_Layer ) :
		CIGUGapLayer( *t_Layer ), m_Layer( t_Layer ), m_ReferenceGas( std::make_shared< CGas >() ),
		m_inTemperature( 0 ), m_outTemperature( 0 ), m_Zin( 0 ), m_Zout( 0 ) {
		( *m_ReferenceGas ) = ( *m_Gas );
		m_ReferenceGas->setTemperatureAndPressure( ReferenceTemperature, m_Pressure );
	}

	CIGUVentilatedGapLayer::CIGUVentilatedGapLayer( CIGUVentilatedGapLayer const& t_Layer ) :
		CState( t_Layer ), CIGUGapLayer( t_Layer ) {
		m_inTemperature = t_Layer.m_inTemperature;
		m_outTemperature = t_Layer.m_outTemperature;
		m_Zin = t_Layer.m_Zin;
		m_Zout = t_Layer.m_Zout;
	}

	double CIGUVentilatedGapLayer::layerTemperature() {
		assert( m_Height != 0 );
		auto cHeight = characteristicHeight();
		auto avTemp = averageTemperature();
		return avTemp - ( cHeight / m_Height ) * ( m_outTemperature - m_inTemperature );
	}

	void CIGUVentilatedGapLayer::setFlowGeometry( double const t_Atop, double const t_Abot,
	                                              AirVerticalDirection const& t_Direction ) {
		m_AirVerticalDirection = t_Direction;
		auto Ain = 0.0;
		auto Aout = 0.0;
		switch ( m_AirVerticalDirection ) {
		case AirVerticalDirection::None:
			// Do nothing. Airflow does not exist
			break;
		case AirVerticalDirection::Up:
			Ain = t_Abot;
			Aout = t_Atop;
			break;
		case AirVerticalDirection::Down:
			Ain = t_Atop;
			Aout = t_Abot;
			break;
		default:
			throw std::runtime_error( "Incorrect assignment for airflow direction." );
			break;
		}

		m_Zin = calcImpedance( Ain );
		m_Zout = calcImpedance( Aout );

		resetCalculated();
	}

	void CIGUVentilatedGapLayer::setFlowTemperatures( double const t_topTemp, double const t_botTemp,
	                                                  AirVerticalDirection const& t_Direction ) {
		m_AirVerticalDirection = t_Direction;
		switch ( m_AirVerticalDirection ) {
		case AirVerticalDirection::None:
			break;
		case AirVerticalDirection::Up:
			m_inTemperature = t_botTemp;
			m_outTemperature = t_topTemp;
			break;
		case AirVerticalDirection::Down:
			m_inTemperature = t_topTemp;
			m_outTemperature = t_botTemp;
			break;
		default:
			throw std::runtime_error( "Incorrect argument for airflow direction." );
			break;
		}

		resetCalculated();
	}

	void CIGUVentilatedGapLayer::setFlowSpeed( double const t_speed ) {
		m_AirSpeed = t_speed;
		resetCalculated();
	}

	double CIGUVentilatedGapLayer::getAirflowReferencePoint( double const t_GapTemperature ) {
		using ConstantsData::GRAVITYCONSTANT;

		auto tiltAngle = M_PI / 180 * ( m_Tilt - 90 );
		auto gapTemperature = layerTemperature();
		auto aProperties = m_ReferenceGas->getGasProperties();
		auto temperatureMultiplier = fabs( gapTemperature - t_GapTemperature ) /
			( gapTemperature * t_GapTemperature );
		return aProperties->m_Density * ReferenceTemperature * GRAVITYCONSTANT * m_Height *
			fabs( cos( tiltAngle ) ) * temperatureMultiplier;
	}

	double CIGUVentilatedGapLayer::bernoullyPressureTerm() const {
		auto aGasProperties = m_Gas->getGasProperties();
		return 0.5 * aGasProperties->m_Density;
	}

	double CIGUVentilatedGapLayer::hagenPressureTerm() const {
		auto aGasProperties = m_Gas->getGasProperties();
		return 12 * aGasProperties->m_Viscosity * m_Height / pow( getThickness(), 2 );
	}

	double CIGUVentilatedGapLayer::pressureLossTerm() const {
		auto aGasProperties = m_Gas->getGasProperties();
		return 0.5 * aGasProperties->m_Density * ( m_Zin + m_Zout );
	}

	double CIGUVentilatedGapLayer::betaCoeff() {
		calculateLayerHeatFlow();
		return exp( -m_Height / characteristicHeight() );
	}

	void CIGUVentilatedGapLayer::smoothEnergyGain( double const qv1, double const qv2 ) {
		auto smooth = ( fabs( qv1 ) + fabs( qv2 ) ) / 2;
		m_LayerGainFlow = smooth;
		if ( m_inTemperature < m_outTemperature ) {
			m_LayerGainFlow = -m_LayerGainFlow;
		}
	}

	void CIGUVentilatedGapLayer::calculateConvectionOrConductionFlow() {
		CIGUGapLayer::calculateConvectionOrConductionFlow();
		if ( !isCalculated() ) {
			ventilatedFlow();
		}
	}

	double CIGUVentilatedGapLayer::characteristicHeight() const {
		auto aProperties = m_Gas->getGasProperties();
		double cHeight = 0;
		// Characteristic height can only be calculated after initialization is performed
		if ( m_ConductiveConvectiveCoeff != 0 ) {
			cHeight = aProperties->m_Density * aProperties->m_SpecificHeat * getThickness() * m_AirSpeed /
				( 4 * m_ConductiveConvectiveCoeff );
		}
		return cHeight;
	}

	double CIGUVentilatedGapLayer::calcImpedance( double const t_A ) const {
		auto impedance = 0.0;

		if ( t_A != 0 ) {
			impedance = pow( m_Width * getThickness() / ( 0.6 * t_A ) - 1, 2 );
		}

		return impedance;
	}

	void CIGUVentilatedGapLayer::ventilatedFlow() {
		auto aProperties = m_Gas->getGasProperties();
		m_LayerGainFlow = aProperties->m_Density * aProperties->m_SpecificHeat * m_AirSpeed * getThickness() *
			m_Width * ( m_inTemperature - m_outTemperature );
	}

}
