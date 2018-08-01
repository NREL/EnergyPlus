
#include <cmath>
#include <vector>
#include <memory>
#include <algorithm>
#include <cassert>
#include <stdexcept>
#include <iostream>

#include "IGU.hpp"
#include "BaseIGULayer.hpp"
#include "IGUSolidLayer.hpp"
#include "IGUGapLayer.hpp"
#include "Surface.hpp"
#include "IGUSolidDeflection.hpp"
#include "IGUGapDeflection.hpp"
#include "IGUVentilatedGapLayer.hpp"
#include "BaseShade.hpp"
#include "Environment.hpp"
#include "WCECommon.hpp"

using namespace FenestrationCommon;

namespace Tarcog {
	CIGU::CIGU( double const t_Width, double const t_Height, double const t_Tilt ) :
		m_Width( t_Width ), m_Height( t_Height ), m_Tilt( t_Tilt ) {
	}

	CIGU::CIGU( CIGU const& t_IGU ) {
		operator=( t_IGU );
	}

	CIGU & CIGU::operator=( CIGU const & t_IGU ) {
		m_Width = t_IGU.m_Width;
		m_Height = t_IGU.m_Height;
		m_Tilt = t_IGU.m_Tilt;
		for ( auto& layer : t_IGU.m_Layers ) {
			const auto aLayer = std::dynamic_pointer_cast< CBaseIGULayer >( layer->clone() );
			addLayer( aLayer );
		}

		return *this;
	}

	CIGU::~CIGU() {
		for ( std::shared_ptr< CBaseIGULayer > layer : getSolidLayers() ) {
			layer->tearDownConnections();
		}
	}

	void CIGU::addLayer( std::shared_ptr< CBaseIGULayer > const& t_Layer ) {

		// pushes only solid layers to array. Gap layers are connected via linked list
		// In case this is first layer then it must be a solid layer in order to create IGU
		if ( getNumOfLayers() == 0 ) {
			if ( std::dynamic_pointer_cast< CIGUSolidLayer >( t_Layer ) != NULL ) {
				m_Layers.push_back( t_Layer );
			}
			else {
				throw std::runtime_error( "First inserted layer must be a solid layer." );
			}
		}
		else {
			auto lastLayer = m_Layers.back();
			if ( std::dynamic_pointer_cast< CIGUSolidLayer >( t_Layer ) !=
				std::dynamic_pointer_cast< CIGUSolidLayer >( lastLayer ) ) {
				m_Layers.push_back( t_Layer );
				lastLayer->connectToBackSide( t_Layer );
			}
			else {
				throw std::runtime_error( "Two adjecent layers in IGU cannot be of same type. "
				                    "IGU must be constructed of array of solid and gap layers." );
			}
		}

		checkForLayerUpgrades( t_Layer );

		t_Layer->setTilt( m_Tilt );
		t_Layer->setWidth( m_Width );
		t_Layer->setHeight( m_Height );
	}

	void CIGU::setTilt( double const t_Tilt ) {
		for ( auto& layer : m_Layers ) {
			layer->setTilt( t_Tilt );
		}
	}

	void CIGU::setWidth( double const t_Width ) {
		for ( auto& layer : m_Layers ) {
			layer->setWidth( t_Width );
		}
	}

	void CIGU::setHeight( double const t_Height ) {
		for ( auto& layer : m_Layers ) {
			layer->setHeight( t_Height );
		}
	}

	void CIGU::setSolarRadiation( double const t_SolarRadiation ) const {
		for ( auto& layer : getSolidLayers() ) {
			layer->setSolarRadiation( t_SolarRadiation );
		}
	}

	std::shared_ptr< CBaseLayer > CIGU::getLayer( Environment const t_Environment ) const {
		std::shared_ptr< CBaseLayer > aLayer = nullptr;
		switch ( t_Environment ) {
		case Environment::Indoor:
			aLayer = m_Layers.back();
			break;
		case Environment::Outdoor:
			aLayer = m_Layers.front();
			break;
		default:
			assert("Incorrect environment selection.");
			break;
		}
		return aLayer;
	}

	std::shared_ptr< std::vector< double > > CIGU::getState() const {

		auto aState = std::make_shared< std::vector< double > >();

		for ( auto& layer : getSolidLayers() ) {
			auto aSurface = layer->getSurface( Side::Front );
			assert( aSurface != nullptr );
			aState->push_back( aSurface->getTemperature() );
			aState->push_back( aSurface->J() );
			aSurface = layer->getSurface( Side::Back );
			assert( aSurface != nullptr );
			aState->push_back( aSurface->J() );
			aState->push_back( aSurface->getTemperature() );
		}

		return aState;
	}

	void CIGU::setState( std::vector< double >& t_State ) const {
		size_t i = 0;
		for ( auto& aLayer : getSolidLayers() ) {
			auto Tf = t_State[ 4 * i ];
			auto Jf = t_State[ 4 * i + 1 ];
			auto Jb = t_State[ 4 * i + 2 ];
			auto Tb = t_State[ 4 * i + 3 ];
			aLayer->setLayerState( Tf, Tb, Jf, Jb );
			++i;
		}
	}

	std::shared_ptr< std::vector< double > > CIGU::getTemperatures() const {
		auto aTemperatures = std::make_shared< std::vector< double > >();

		for ( auto const& layer : getSolidLayers() ) {
			for ( auto aSide : EnumSide() ) {
				auto aSurface = layer->getSurface( aSide );
				assert( aSurface != nullptr );
				aTemperatures->push_back( aSurface->getTemperature() );
			}
		}

		return aTemperatures;
	}

	std::shared_ptr< std::vector< double > > CIGU::getRadiosities() const {
		auto aRadiosities = std::make_shared< std::vector< double > >();

		for ( auto const& layer : getSolidLayers() ) {
			for ( auto aSide : EnumSide() ) {
				auto aSurface = layer->getSurface( aSide );
				assert( aSurface != nullptr );
				aRadiosities->push_back( aSurface->J() );
			}
		}

		return aRadiosities;
	}

	std::shared_ptr< std::vector< double > > CIGU::getMaxDeflections() const {
		auto aMaxDeflections = std::make_shared< std::vector< double > >();

		for ( auto const& layer : getSolidLayers() ) {
			aMaxDeflections->push_back( layer->getMaxDeflection() );
		}

		return aMaxDeflections;
	}

	std::shared_ptr< std::vector< double > > CIGU::getMeanDeflections() const {
		auto aMeanDeflections = std::make_shared< std::vector< double > >();

		for ( auto const& layer : getSolidLayers() ) {
			aMeanDeflections->push_back( layer->getMeanDeflection() );
		}

		return aMeanDeflections;
	}

	double CIGU::getThickness() const {
		auto totalWidth = 0.0;

		for ( auto& layer : m_Layers ) {
			totalWidth += layer->getThickness();
		}

		return totalWidth;
	}

	double CIGU::getTilt() const {
		return m_Tilt;
	}

	double CIGU::getWidth() const {
		return m_Width;
	}

	double CIGU::getHeight() const {
		return m_Height;
	}

	size_t CIGU::getNumOfLayers() const {
		return ( m_Layers.size() + 1 ) / 2;
	}

	double CIGU::getVentilationFlow( Environment const t_Environment ) const {
		auto size = m_Layers.size();
		std::map< Environment, size_t > envLayer = { { Environment::Indoor, size - 2 }, { Environment::Outdoor, 1 } };
		return m_Layers[ envLayer.at( t_Environment ) ]->getGainFlow();
	}

	void CIGU::setInitialGuess( std::vector< double > const& t_Guess ) const {
		if ( 2 * getNumOfLayers() != t_Guess.size() ) {
			std::cout << "Number of temperatures in initial guess cannot fit number of layers."
				"Program will use initial guess instead" << std::endl;
		}
		else {
			size_t Index = 0;
			for ( auto& aLayer : getSolidLayers() ) {
				for ( auto aSide : EnumSide() ) {
					auto aSurface = aLayer->getSurface( aSide );
					aSurface->initializeStart( t_Guess[ Index ] );
					++Index;
				}
			}
		}
	}

	void CIGU::setDeflectionProperties( double const t_Tini, double const t_Pini ) {
		// Simply decorating layers in a list with new behavior
		auto aVector = getSolidLayers();
		// deflection properties of the IGU
		auto Lmean = Ldmean();
		auto Lmax = Ldmax();

		for ( auto& aLayer : getSolidLayers() ) {
			// Deflection could aslo be decorated (created) outside in which case program already have a layer as
			// deflection layer. If that is not done then layer must be decorated with defalut deflection
			// properties
			std::shared_ptr< CIGUSolidLayerDeflection > aDeflectionLayer = nullptr;
			if ( std::dynamic_pointer_cast< CIGUSolidLayerDeflection >( aLayer ) == NULL ) {
				aDeflectionLayer = std::make_shared< CIGUSolidLayerDeflection >( *aLayer );
			}
			else {
				aDeflectionLayer = std::dynamic_pointer_cast< CIGUSolidLayerDeflection >( aLayer );
			}
			replaceLayer( aLayer, std::make_shared< CIGUDeflectionTempAndPressure >( aDeflectionLayer, Lmax, Lmean ) );
		}
		for ( std::shared_ptr< CIGUGapLayer >& aLayer : getGapLayers() ) {
			replaceLayer( aLayer, std::make_shared< CIGUGapLayerDeflection >( *aLayer, t_Tini, t_Pini ) );
		}
	}

	void CIGU::setDeflectionProperties( std::vector< double > const& t_MeasuredDeflections ) {
		if ( t_MeasuredDeflections.size() != getNumOfLayers() - 1 ) {
			throw std::runtime_error( "Number of measured deflection values must be equal to number of gaps." );
		}

		auto nominator = 0.0;
		for ( size_t i = 0; i < t_MeasuredDeflections.size(); ++i ) {
			auto SumL = 0.0;
			for ( auto j = i; j < t_MeasuredDeflections.size(); ++j ) {
				SumL += getGapLayers()[ j ]->getThickness() - t_MeasuredDeflections[ j ];
			}
			auto aDefLayer = CIGUSolidLayerDeflection( *getSolidLayers()[ i ] );
			nominator += SumL * aDefLayer.flexuralRigidity();
		}

		auto denominator = 0.0;
		for ( auto i = 0u; i < getSolidLayers().size(); ++i ) {
			auto aDefLayer = CIGUSolidLayerDeflection( *getSolidLayers()[ i ] );
			denominator += aDefLayer.flexuralRigidity();
		}

		// First need to calculate new deflections before applying them. Applying them right away will
		// cause that next gap width calculation will already have included one surface deflected
		auto LDefNMax = nominator / denominator;
		auto deflectionRatio = Ldmean() / Ldmax();

		std::vector< double > LDefMax;
		LDefMax.push_back( LDefNMax );
		for ( auto i = getNumOfLayers() - 1; i > 0; --i ) {
			LDefNMax = t_MeasuredDeflections[ i - 1 ] - getGapLayers()[ i - 1 ]->getThickness() + LDefNMax;
			LDefMax.insert( LDefMax.begin(), LDefNMax );
		}

		for ( auto i = 0u; i < getNumOfLayers(); ++i ) {
			LDefNMax = LDefMax[ i ];
			auto LDefNMean = deflectionRatio * LDefNMax;
			auto aLayer = getSolidLayers()[ i ];
			auto aDefLayer = std::make_shared< CIGUSolidLayerDeflection >( *aLayer );
			aDefLayer = std::make_shared< CIGUDeflectionMeasuread >( aDefLayer, LDefNMean, LDefNMax );
			replaceLayer( aLayer, aDefLayer );
		}

	}

	void CIGU::replaceLayer( std::shared_ptr< CBaseIGULayer > const& t_Original,
	                         std::shared_ptr< CBaseIGULayer > const& t_Replacement ) {
		size_t index = find( m_Layers.begin(), m_Layers.end(), t_Original ) - m_Layers.begin();
		m_Layers[ index ] = t_Replacement;
		if ( index > 0 ) {
			m_Layers[ index - 1 ]->connectToBackSide( t_Replacement );
		}
		if ( index < m_Layers.size() - 1 ) {
			t_Replacement->connectToBackSide( m_Layers[ index + 1 ] );
		}
	}

	void CIGU::checkForLayerUpgrades( std::shared_ptr< CBaseIGULayer > const& t_Layer ) {
		if ( std::dynamic_pointer_cast< CIGUShadeLayer >( t_Layer ) != nullptr ) {
			if ( std::dynamic_pointer_cast< CIGUGapLayer >( t_Layer->getPreviousLayer() ) != nullptr ) {
				auto newLayer = std::make_shared< CIGUVentilatedGapLayer >( std::dynamic_pointer_cast< CIGUGapLayer >( t_Layer->getPreviousLayer() ) );
				replaceLayer( std::dynamic_pointer_cast< CIGUGapLayer >( t_Layer->getPreviousLayer() ), newLayer );
			}
		}
		if ( std::dynamic_pointer_cast< CIGUGapLayer >( t_Layer ) != nullptr ) {
			if ( std::dynamic_pointer_cast< CIGUShadeLayer >( t_Layer->getPreviousLayer() ) != nullptr ) {
				auto newLayer = std::make_shared< CIGUVentilatedGapLayer >( std::dynamic_pointer_cast< CIGUGapLayer >( t_Layer ) );
				replaceLayer( std::dynamic_pointer_cast< CIGUGapLayer >( t_Layer ), newLayer );
			}
		}
	}

	double CIGU::Ldmean() const {
		using ConstantsData::PI;

		auto coeff = 16 / ( pow( PI, 6 ) );
		auto totalSum = 0.0;
		for ( auto m = 1; m <= 5; m += 2 ) {
			for ( auto n = 1; n <= 5; n += 2 ) {
				auto nomin = 4.0;
				auto denom = m * m * n * n * PI * PI * pow( pow( m / m_Width, 2 ) + pow( n / m_Height, 2 ), 2 );
				totalSum += nomin / denom;
			}
		}
		return coeff * totalSum;
	}

	double CIGU::Ldmax() const {
		using ConstantsData::PI;

		auto coeff = 16 / ( pow( PI, 6 ) );
		auto totalSum = 0.0;
		for ( auto m = 1; m <= 5; m += 2 ) {
			for ( auto n = 1; n <= 5; n += 2 ) {
				auto nomin = sin( m * PI / 2 ) * sin( n * PI / 2 );
				auto denom = m * n * pow( pow( m / m_Width, 2 ) + pow( n / m_Height, 2 ), 2 );
				totalSum += nomin / denom;
			}
		}
		return coeff * totalSum;
	}

	std::vector< std::shared_ptr< CIGUSolidLayer > > CIGU::getSolidLayers() const {
		std::vector< std::shared_ptr< CIGUSolidLayer > > aVect;
		for ( auto const& aLayer : m_Layers ) {
			if ( std::dynamic_pointer_cast< CIGUSolidLayer >( aLayer ) != nullptr ) {
				aVect.push_back( std::dynamic_pointer_cast< CIGUSolidLayer >( aLayer ) );
			}
		}
		return aVect;
	}

	std::vector< std::shared_ptr< CIGUGapLayer > > CIGU::getGapLayers() const {
		std::vector< std::shared_ptr< CIGUGapLayer > > aVect;
		for ( auto const& aLayer : m_Layers ) {
			if ( std::dynamic_pointer_cast< CIGUGapLayer >( aLayer ) != nullptr ) {
				aVect.push_back( std::dynamic_pointer_cast< CIGUGapLayer >( aLayer ) );
			}
		}
		return aVect;
	}

	std::vector< std::shared_ptr< CBaseIGULayer > > CIGU::getLayers() const {
		return m_Layers;
	}

}
