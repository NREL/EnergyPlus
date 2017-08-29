#include <memory>
#define _USE_MATH_DEFINES
#include <math.h>

#include "Constants.hpp"
#include "GasItem.hpp"
#include "GasData.hpp"
#include "GasSetting.hpp"



namespace Gases {

	CGasItem::CGasItem() : m_Fraction( 1.0 ), m_GasData( new CGasData() ) {
		initialize();
	}

	CGasItem::CGasItem( CGasItem const& t_GasItem ) :
		m_Temperature( t_GasItem.m_Temperature ),
		m_Pressure( t_GasItem.m_Pressure ),
		m_Fraction( t_GasItem.m_Fraction ),
		m_GasProperties( std::make_shared< GasProperties >() ),
		m_FractionalGasProperties( std::make_shared< GasProperties >() ),
		m_GasData( new CGasData( *t_GasItem.m_GasData.get() ) ) {
		( *m_FractionalGasProperties ) = ( *t_GasItem.m_FractionalGasProperties );
		( *m_GasProperties ) = ( *t_GasItem.m_GasProperties );
	}

	CGasItem::CGasItem( double const t_Fraction, CGasData const& t_GasData ) :
		m_Fraction( t_Fraction ), m_GasData( new CGasData( t_GasData ) ) {
		initialize();
	}

	CGasItem& CGasItem::operator=( CGasItem const& t_GasItem ) {
		m_Fraction = t_GasItem.m_Fraction;
		m_Pressure = t_GasItem.m_Pressure;
		m_Temperature = t_GasItem.m_Temperature;
		*m_GasData = *t_GasItem.m_GasData;
		( *m_FractionalGasProperties ) = ( *t_GasItem.m_FractionalGasProperties );
		( *m_GasProperties ) = ( *t_GasItem.m_GasProperties );

		return *this;
	}

	void CGasItem::fillStandardPressureProperites() const {
		using ConstantsData::UNIVERSALGASCONSTANT;
		m_GasProperties->m_ThermalConductivity = m_GasData->getPropertyValue( CoeffType::cCond, m_Temperature );
		m_GasProperties->m_Viscosity = m_GasData->getPropertyValue( CoeffType::cVisc, m_Temperature );
		m_GasProperties->m_SpecificHeat = m_GasData->getPropertyValue( CoeffType::cCp, m_Temperature );
		m_GasProperties->m_MolecularWeight = m_GasData->getMolecularWeight();
		m_GasProperties->m_Density = m_Pressure * m_GasProperties->m_MolecularWeight /
			( UNIVERSALGASCONSTANT * m_Temperature );
		m_GasProperties->calculateAlphaAndPrandl();
	}

	void CGasItem::flllVacuumPressureProperties() const {
		using ConstantsData::UNIVERSALGASCONSTANT;
		auto const alpha1 = 0.79;
		auto const alpha2 = 0.79;
		auto const alpha = alpha1 * alpha2 / ( alpha2 + alpha1 * ( 1 - alpha2 ) );
		auto const specificHeatRatio = m_GasData->getSpecificHeatRatio();
		if ( specificHeatRatio == 1 ) {
			throw std::runtime_error( "Specific heat ratio of a gas cannot be equal to one." );
		}
		auto const mWght = m_GasData->getMolecularWeight();
		auto B = alpha * ( specificHeatRatio + 1 ) / ( specificHeatRatio - 1 );
		B *= sqrt( UNIVERSALGASCONSTANT / ( 8 * M_PI * mWght * m_Temperature ) );
		m_GasProperties->m_ThermalConductivity = B * m_Pressure;
		m_GasProperties->m_Viscosity = 0;
		m_GasProperties->m_SpecificHeat = 0;
		m_GasProperties->m_MolecularWeight = mWght;
		m_GasProperties->m_Density = 0;
	}

	void CGasItem::initialize() {
		m_Temperature = DefaultTemperature;
		m_Pressure = DefaultPressure;
		m_FractionalGasProperties = std::make_shared< GasProperties >();
		m_GasProperties = std::make_shared< GasProperties >();
	}

	double CGasItem::getFraction() const {
		return m_Fraction;
	}

	void CGasItem::resetCalculatedProperties() const {
		m_GasProperties->m_PropertiesCalculated = false;
		m_FractionalGasProperties->m_PropertiesCalculated = false;
	}

	void CGasItem::setTemperature( double const t_Temperature ) {
		m_Temperature = t_Temperature;
		resetCalculatedProperties();
	}

	void CGasItem::setPressure( double const t_Pressure ) {
		m_Pressure = t_Pressure;
		resetCalculatedProperties();
	}

	std::shared_ptr< GasProperties > CGasItem::getGasProperties() const {

		if ( !m_GasProperties->m_PropertiesCalculated ) {
			auto aSettings = CGasSettings::instance();

			if ( m_Pressure > aSettings.getVacuumPressure() ) {
				fillStandardPressureProperites();
			}
			else {
				flllVacuumPressureProperties();
			}
			m_GasProperties->m_PropertiesCalculated = true;
		}

		return m_GasProperties;
	}

	std::shared_ptr< GasProperties > CGasItem::getFractionalGasProperties() const {
		if ( !m_FractionalGasProperties->m_PropertiesCalculated ) {

			auto itemGasProperties = getGasProperties();

			// update for fractional data
			m_FractionalGasProperties->m_ThermalConductivity = itemGasProperties->m_ThermalConductivity * m_Fraction;
			m_FractionalGasProperties->m_Viscosity = itemGasProperties->m_Viscosity * m_Fraction;
			m_FractionalGasProperties->m_SpecificHeat = itemGasProperties->m_SpecificHeat * m_Fraction;
			m_FractionalGasProperties->m_MolecularWeight = itemGasProperties->m_MolecularWeight * m_Fraction;
			m_FractionalGasProperties->m_Density = itemGasProperties->m_Density * m_Fraction;
			m_FractionalGasProperties->m_Alpha = itemGasProperties->m_Alpha * m_Fraction;
			m_FractionalGasProperties->m_PrandlNumber = itemGasProperties->m_PrandlNumber * m_Fraction;
		}

		return m_FractionalGasProperties;
	}

	bool CGasItem::operator==( CGasItem const& rhs ) const {
		return m_Temperature == rhs.m_Temperature &&
			m_Pressure == rhs.m_Pressure &&
			m_Fraction == rhs.m_Fraction &&
			m_GasProperties == rhs.m_GasProperties &&
			m_FractionalGasProperties == rhs.m_FractionalGasProperties &&
			m_GasData == rhs.m_GasData;
	}

	bool CGasItem::operator!=( CGasItem const& rhs ) const {
		return !( rhs == *this );
	}

}
