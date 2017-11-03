#include <cmath>
#include <stdexcept>
#include <cassert>
#include <iostream>

#include "BaseShade.hpp"
#include "IGUGapLayer.hpp"
#include "Surface.hpp"
#include "NusseltNumber.hpp"
#include "WCEGases.hpp"
#include "WCECommon.hpp"


using namespace Gases;
using namespace FenestrationCommon;

namespace Tarcog {

	class CBaseLayer;

	CIGUGapLayer::CIGUGapLayer( double const t_Thickness, double const t_Pressure ) :
		CState(), CBaseIGULayer( t_Thickness ), CGasLayer( t_Pressure ) {

	}

	CIGUGapLayer::CIGUGapLayer( double const t_Thickness, double const t_Pressure,
	                            std::shared_ptr< CGas > const& t_Gas ) :
		CState(), CBaseIGULayer( t_Thickness ), CGasLayer( t_Pressure, t_Gas ) {
		assert( m_Gas != nullptr );
	}

	CIGUGapLayer::CIGUGapLayer( CIGUGapLayer const& t_Layer ) :
		CState( t_Layer ), CBaseIGULayer( t_Layer ), CGasLayer( t_Layer ) {

	}

	void CIGUGapLayer::connectToBackSide( std::shared_ptr< CBaseLayer > const& t_Layer ) {
		CBaseLayer::connectToBackSide( t_Layer );
		m_Surface[ Side::Back ] = t_Layer->getSurface( Side::Front );
	}

	void CIGUGapLayer::initializeStateVariables() {
		CGasLayer::initializeStateVariables();
	}

	void CIGUGapLayer::calculateConvectionOrConductionFlow() {
		checkNextLayer();
		if ( !isCalculated() ) {
			if ( getThickness() == 0 ) {
				throw std::runtime_error( "Layer thickness is set to zero." );
			}

			convectiveH();
		}
	}

	void CIGUGapLayer::checkNextLayer() const {
		if ( m_NextLayer != nullptr ) {
			m_NextLayer->getGainFlow();
		}
	}

	double CIGUGapLayer::layerTemperature() {
		return averageTemperature();
	}

	double CIGUGapLayer::calculateRayleighNumber() {
		using ConstantsData::GRAVITYCONSTANT;

		auto tGapTemperature = layerTemperature();
		auto deltaTemp = std::abs( getSurface( Side::Back )->getTemperature() -
		                      getSurface( Side::Front )->getTemperature() );

		auto aProperties = m_Gas->getGasProperties();

		double Ra = 0;
		if ( aProperties->m_Viscosity != 0 ) { // if viscosity is zero then it is vacuum
			Ra = GRAVITYCONSTANT * pow( getThickness(), 3 ) * deltaTemp *
				aProperties->m_SpecificHeat * pow( aProperties->m_Density, 2 ) /
				( tGapTemperature * aProperties->m_Viscosity * aProperties->m_ThermalConductivity );
		}

		return Ra;
	}

	double CIGUGapLayer::aspectRatio() const {
		if ( getThickness() == 0 ) {
			throw std::runtime_error( "Gap thickness is set to zero." );
		}
		return m_Height / getThickness();
	}

	double CIGUGapLayer::convectiveH() {
		auto tGapTemperature = layerTemperature();
		m_Gas->setTemperatureAndPressure( tGapTemperature, getPressure() );
		auto Ra = calculateRayleighNumber();
		auto Asp = aspectRatio();
		auto nusseltNumber = std::make_shared< CNusseltNumber >();
		auto aProperties = m_Gas->getGasProperties();
		if ( aProperties->m_Viscosity != 0 ) {
			m_ConductiveConvectiveCoeff = nusseltNumber->calculate( m_Tilt, Ra, Asp ) *
				aProperties->m_ThermalConductivity / getThickness();
		}
		else { // vacuum state
			m_ConductiveConvectiveCoeff = aProperties->m_ThermalConductivity;
		}
		if ( m_AirSpeed != 0 ) {
			m_ConductiveConvectiveCoeff = m_ConductiveConvectiveCoeff + 2 * m_AirSpeed;
		}

		return m_ConductiveConvectiveCoeff;
	}

	double CIGUGapLayer::getGasTemperature() {
		return layerTemperature();
	}

	double CIGUGapLayer::averageTemperature() const {
		double aveTemp = 0;
		if ( areSurfacesInitalized() ) {
			aveTemp = ( getSurface( Side::Front )->getTemperature() +
				getSurface( Side::Back )->getTemperature() ) / 2;
		}

		return aveTemp;
	}

	double CIGUGapLayer::getPressure() {
		return m_Pressure;
	}

	std::shared_ptr< CBaseLayer > CIGUGapLayer::clone() const {
		return std::make_shared< CIGUGapLayer >( *this );
	}

}
