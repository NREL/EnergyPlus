#include <stdexcept>
#include <math.h>
#include "Gas.hpp"
#include "GasData.hpp"
#include "GasItem.hpp"
#include "GasSetting.hpp"



namespace Gases {

	CGas::CGas() : m_SimpleProperties( std::make_shared< GasProperties >() ),
	               m_Properties( std::make_shared< GasProperties >() ), m_Pressure( DefaultPressure ) {
		// create default gas to be Air
		auto Air = CGasItem();
		m_GasItem.push_back( Air );
		m_DefaultGas = true;
	}

	CGas::CGas( CGas const& t_Gas ) :
		m_GasItem( t_Gas.m_GasItem ), m_SimpleProperties( t_Gas.m_SimpleProperties ),
		m_Properties( t_Gas.m_Properties ), m_DefaultGas( t_Gas.m_DefaultGas ),
		m_Pressure( t_Gas.m_Pressure ) {
		m_GasItem.clear();
		for ( auto item : t_Gas.m_GasItem ) {
			m_GasItem.push_back( item );
		}
	}

	void CGas::addGasItem( CGasItem const& t_GasItem ) {
		// Need to remove default since user wants to create their own Gases
		if ( m_DefaultGas ) {
			m_GasItem.clear();
			m_DefaultGas = false;
		}
		m_GasItem.push_back( t_GasItem );
	}

	double CGas::totalPercent() {
		auto totalPercent = 0.0;

		for ( auto& it : m_GasItem ) {
			totalPercent += it.getFraction();
		}

		return totalPercent;
	}

	void CGas::setTemperatureAndPressure( double const t_Temperature, double const t_Pressure ) {
		m_Pressure = t_Pressure;
		for ( auto& item : m_GasItem ) {
			item.setTemperature( t_Temperature );
			item.setPressure( t_Pressure );
		}
	}

	std::shared_ptr< GasProperties > CGas::getSimpleGasProperties() {
		*m_SimpleProperties = *( ( m_GasItem )[ 0 ].getFractionalGasProperties() );
		for ( auto it = next( m_GasItem.begin() ); it != m_GasItem.end(); ++it ) {
			*m_SimpleProperties += *( it->getFractionalGasProperties() );
		}

		return m_SimpleProperties;
	}

	std::shared_ptr< GasProperties > CGas::getGasProperties() {
		auto aSettings = CGasSettings::instance();
		return aSettings.getVacuumPressure() < m_Pressure ? getStandardPressureGasProperties()
			       : getVacuumPressureGasProperties();
	}

	std::shared_ptr< GasProperties > CGas::getStandardPressureGasProperties() {
		auto simpleProperties = getSimpleGasProperties();

		// coefficients for intermediate calculations
		std::vector< std::vector< double > > miItem;
		std::vector< std::vector< double > > lambdaPrimItem;
		std::vector< std::vector< double > > lambdaSecondItem;

		auto gasSize = m_GasItem.size();
		auto counter = 0;

		miItem.resize( gasSize );
		lambdaPrimItem.resize( gasSize );
		lambdaSecondItem.resize( gasSize );

		for ( auto& primaryIt : m_GasItem ) {
			for ( auto& secondaryIt : m_GasItem ) {
				if ( primaryIt != secondaryIt ) {
					miItem[ counter ].push_back( viscDenomTwoGases( primaryIt, secondaryIt ) );
					lambdaPrimItem[ counter ].push_back( lambdaPrimDenomTwoGases( primaryIt, secondaryIt ) );
					lambdaSecondItem[ counter ].push_back( lambdaSecondDenomTwoGases( primaryIt, secondaryIt ) );
				}
				else {
					miItem[ counter ].push_back( 0 );
					lambdaPrimItem[ counter ].push_back( 0 );
					lambdaSecondItem[ counter ].push_back( 0 );
				}
			}
			counter++;
		}

		double miMix( 0 );
		double lambdaPrimMix( 0 );
		double lambdaSecondMix( 0 );
		double cpMix( 0 );

		counter = 0;
		for ( auto& it : m_GasItem ) {
			auto itGasProperties = it.getGasProperties();
			auto lambdaPrim( itGasProperties->getLambdaPrim() );
			auto lambdaSecond( itGasProperties->getLambdaSecond() );

			auto sumMix = 1.0;
			for ( size_t i = 0; i < gasSize; ++i ) {
				sumMix += miItem[ counter ][ i ];
			}

			miMix += itGasProperties->m_Viscosity / sumMix;

			sumMix = 1.0;
			for ( size_t i = 0; i < gasSize; ++i ) {
				sumMix += lambdaPrimItem[ counter ][ i ];
			}

			lambdaPrimMix += lambdaPrim / sumMix;

			sumMix = 1.0;
			for ( size_t i = 0; i < gasSize; ++i ) {
				sumMix += lambdaSecondItem[ counter ][ i ];
			}

			lambdaSecondMix += lambdaSecond / sumMix;

			cpMix += itGasProperties->m_SpecificHeat * it.getFraction() * itGasProperties->m_MolecularWeight;
			++counter;
		}

		m_Properties->m_ThermalConductivity = lambdaPrimMix + lambdaSecondMix;
		m_Properties->m_Viscosity = miMix;
		m_Properties->m_SpecificHeat = cpMix / simpleProperties->m_MolecularWeight;
		m_Properties->m_Density = simpleProperties->m_Density;
		m_Properties->m_MolecularWeight = simpleProperties->m_MolecularWeight;
		m_Properties->calculateAlphaAndPrandl();

		return m_Properties;
	}

	std::shared_ptr< GasProperties > CGas::getVacuumPressureGasProperties() {
		return getSimpleGasProperties();
	}

	// This implements equation 63 (ISO 15099)
	double CGas::viscTwoGases( GasProperties const& t_Gas1Properties,
	                           GasProperties const& t_Gas2Properties ) const {

		if ( t_Gas1Properties.m_Viscosity == 0 || t_Gas2Properties.m_Viscosity == 0 ) {
			throw std::runtime_error( "Viscosity of the gas component in Gases is equal to zero." );
		}
		if ( ( t_Gas1Properties.m_MolecularWeight == 0 ) || ( t_Gas2Properties.m_MolecularWeight == 0 ) ) {
			throw std::runtime_error( "Molecular weight of the gas component in Gases is equal to zero." );
		}

		auto uFraction = t_Gas1Properties.m_Viscosity / t_Gas2Properties.m_Viscosity;
		auto weightFraction = t_Gas1Properties.m_MolecularWeight / t_Gas2Properties.m_MolecularWeight;
		auto nominator = pow( ( 1 + pow( uFraction, 0.5 ) * pow( 1 / weightFraction, 0.25 ) ), 2 );
		auto denominator = 2 * sqrt( 2.0 ) * pow( 1 + weightFraction, 0.5 );

		if ( denominator == 0 ) {
			throw std::runtime_error( "Dynamic viscosity coefficient is gas mixture is calculated to be zero." );
		}

		return nominator / denominator;

	}

	// Implementation of sum items in denominator of equation 62 (ISO15099)
	double CGas::viscDenomTwoGases( CGasItem& t_GasItem1, CGasItem& t_GasItem2 ) const {

		auto phiValue = viscTwoGases( *t_GasItem1.getGasProperties(), *t_GasItem2.getGasProperties() );
		if ( ( t_GasItem1.getFraction() == 0 ) || ( t_GasItem2.getFraction() == 0 ) ) {
			throw std::runtime_error( "Fraction of gas component in gas mixture is set to be equal to zero." );
		}

		return ( t_GasItem2.getFraction() / t_GasItem1.getFraction() ) * phiValue;

	}

	// This implements equation 66 (ISO 15099)
	double CGas::lambdaPrimTwoGases( GasProperties const& t_Gas1Properties,
	                                 GasProperties const& t_Gas2Properties ) const {

		if ( ( t_Gas1Properties.m_MolecularWeight == 0 ) || ( t_Gas2Properties.m_MolecularWeight == 0 ) ) {
			throw std::runtime_error( "Molecular weight of the gas component in Gases is equal to zero." );
		}

		auto item1 = lambdaSecondTwoGases( t_Gas1Properties, t_Gas2Properties );
		auto item2 = 1 + 2.41 * ( ( t_Gas1Properties.m_MolecularWeight - t_Gas2Properties.m_MolecularWeight ) *
			( t_Gas1Properties.m_MolecularWeight - 0.142 * t_Gas2Properties.m_MolecularWeight ) /
			pow( ( t_Gas1Properties.m_MolecularWeight + t_Gas2Properties.m_MolecularWeight ), 2 ) );

		return item1 * item2;

	}

	// This implements equation 68 (ISO 15099)
	double CGas::lambdaSecondTwoGases( GasProperties const& t_Gas1Properties,
	                                   const GasProperties& t_Gas2Properties ) const {

		if ( ( t_Gas1Properties.getLambdaPrim() == 0 ) || ( t_Gas2Properties.getLambdaPrim() == 0 ) ) {
			throw std::runtime_error( "Primary thermal conductivity (lambda prim) of the gas component in Gases is equal to zero." );
		}

		if ( ( t_Gas1Properties.m_MolecularWeight == 0 ) || ( t_Gas2Properties.m_MolecularWeight == 0 ) ) {
			throw std::runtime_error( "Molecular weight of the gas component in Gases is equal to zero." );
		}

		auto tFraction = t_Gas1Properties.getLambdaPrim() / t_Gas2Properties.getLambdaPrim();
		auto weightFraction = t_Gas1Properties.m_MolecularWeight / t_Gas2Properties.m_MolecularWeight;
		auto nominator = pow( ( 1 + pow( tFraction, 0.5 ) * pow( weightFraction, 0.25 ) ), 2 );
		auto denominator = 2 * sqrt( 2.0 ) * pow( ( 1 + weightFraction ), 0.5 );

		if ( denominator == 0 ) {
			throw std::runtime_error( "Thermal conductivity coefficient in gas mixture is calculated to be zero." );
		}

		return nominator / denominator;

	}

	// Implementation of sum items in denominator of equation 65 (ISO15099)
	double CGas::lambdaPrimDenomTwoGases( CGasItem& t_GasItem1, CGasItem& t_GasItem2 ) const {

		auto phiValue = lambdaPrimTwoGases( *t_GasItem1.getGasProperties(), *t_GasItem2.getGasProperties() );

		if ( ( t_GasItem1.getFraction() == 0 ) || ( t_GasItem2.getFraction() == 0 ) ) {
			throw std::runtime_error( "Fraction of gas component in gas mixture is set to be equal to zero." );
		}

		return ( t_GasItem2.getFraction() / t_GasItem1.getFraction() ) * phiValue;

	}

	// Implementation of sum items in denominator of equation 67 (ISO15099)
	double CGas::lambdaSecondDenomTwoGases( CGasItem& t_GasItem1, CGasItem& t_GasItem2 ) const {

		auto phiValue = lambdaSecondTwoGases( *t_GasItem1.getGasProperties(), *t_GasItem2.getGasProperties() );

		if ( ( t_GasItem1.getFraction() == 0 ) || ( t_GasItem2.getFraction() == 0 ) ) {
			throw std::runtime_error( "Fraction of gas component in gas mixture is set to be equal to zero." );
		}

		return ( t_GasItem2.getFraction() / t_GasItem1.getFraction() ) * phiValue;

	}

	CGas& CGas::operator=( CGas const& t_Gas ) {
		m_GasItem.clear();
		for ( auto item : t_Gas.m_GasItem ) {
			m_GasItem.push_back( item );
		}
		m_SimpleProperties = t_Gas.m_SimpleProperties;
		m_Properties = t_Gas.m_Properties;
		m_DefaultGas = t_Gas.m_DefaultGas;

		return *this;
	}

	bool CGas::operator==( CGas const& rhs ) const {
		return m_GasItem == rhs.m_GasItem &&
			m_SimpleProperties == rhs.m_SimpleProperties &&
			m_Properties == rhs.m_Properties &&
			m_DefaultGas == rhs.m_DefaultGas &&
			m_Pressure == rhs.m_Pressure;
	}

	bool CGas::operator!=( CGas const& rhs ) const {
		return !( rhs == *this );
	}

}
