
#include <cmath>
#include <cassert>
#include <stdexcept>

#include "EquivalentBSDFLayer.hpp"
#include "EquivalentBSDFLayerSingleBand.hpp"
#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"

using namespace FenestrationCommon;
using namespace SingleLayerOptics;

namespace MultiLayerOptics {

	CEquivalentBSDFLayer::CEquivalentBSDFLayer( std::vector< double > const& t_CommonWavelengths,
	                                            const std::shared_ptr< CBSDFLayer >& t_Layer ) :
		m_CombinedLayerWavelengths( t_CommonWavelengths ),
		m_Calculated( false ) {
		if ( t_Layer == nullptr ) {
			throw std::runtime_error( "Equivalent BSDF Layer must contain valid layer." );
		}

		// Create layers for each wavelength
		m_LayersWL = std::make_shared< std::vector< std::shared_ptr< CEquivalentBSDFLayerSingleBand > > >();

		// Lambda matrix from spectral results. Same lambda is valid for any wavelength
		m_Lambda = t_Layer->getResults()->lambdaMatrix();

		addLayer( t_Layer );

	}

	void CEquivalentBSDFLayer::addLayer( const std::shared_ptr< CBSDFLayer >& t_Layer ) {

		// t_Layer->setSourceData( m_SolarRadiation );

		m_Layer.push_back( t_Layer );

		updateWavelengthLayers( t_Layer );

	}

	std::shared_ptr< const CBSDFDirections > CEquivalentBSDFLayer::getDirections( const BSDFHemisphere t_Side ) const {
		return m_Layer[ 0 ]->getDirections( t_Side );
	}

	std::vector< double > CEquivalentBSDFLayer::getCommonWavelengths() const {
		return m_CombinedLayerWavelengths;
	}

	std::shared_ptr< CMatrixSeries > CEquivalentBSDFLayer::getTotalA( const Side t_Side ) {
		if ( !m_Calculated ) {
			calculate();
		}
		return m_TotA.at( t_Side );
	}

	std::shared_ptr< CMatrixSeries > CEquivalentBSDFLayer::getTotal(
		const Side t_Side, const PropertySimple t_Property ) {
		if ( !m_Calculated ) {
			calculate();
		}
		return m_Tot.at( std::make_pair( t_Side, t_Property ) );
	}

	void CEquivalentBSDFLayer::setSolarRadiation( const std::shared_ptr< CSeries >& t_SolarRadiation ) {
		// Need to recreate wavelenght by wavelength layers
		m_LayersWL->clear();
		for ( std::shared_ptr< CBSDFLayer > aLayer : m_Layer ) {
			aLayer->setSourceData( t_SolarRadiation );
			updateWavelengthLayers( aLayer );
		}
		m_Calculated = false;
	}

	void CEquivalentBSDFLayer::calculate() {
		size_t matrixSize = m_Lambda->getSize();
		size_t numberOfLayers = ( *m_LayersWL )[ 0 ]->getNumberOfLayers();

		for ( Side aSide : EnumSide() ) {
			m_TotA[ aSide ] = std::make_shared< CMatrixSeries >( numberOfLayers, matrixSize );
			for ( PropertySimple aProperty : EnumPropertySimple() ) {
				m_Tot[ std::make_pair( aSide, aProperty ) ] = std::make_shared< CMatrixSeries >( matrixSize, matrixSize );
			}
		}

		// Calculate total transmitted solar per matrix and perform integration over each wavelength
		size_t WLsize = m_CombinedLayerWavelengths.size();

		// // This is for multithread calculations.
		// size_t numOfThreads = size_t( thread::hardware_concurrency() - 2 );
		// size_t step = WLsize / numOfThreads;
		// std::vector< std::shared_ptr< thread > > aThreads = std::vector< std::shared_ptr< thread > >( numOfThreads );
		// 
		// size_t startNum = 0;
		// size_t endNum = step;
		// 
		// for( size_t i = 0; i < numOfThreads; ++i ) {
		//   if( i == numOfThreads - 1 ) {
		//     endNum = WLsize;
		//   }
		// 
		//   aThreads[ i ] = std::make_shared< thread >( &CEquivalentBSDFLayer::calculateWavelengthProperties, *this,
		//    numberOfLayers, startNum, endNum );
		//   
		//   startNum += step;
		//   endNum += step;
		// }
		// 
		// for( size_t i = 0; i < numOfThreads; ++i ) {
		//   aThreads[ i ]->join();
		// }
		// 
		// // End of multithreaded calculations.


		calculateWavelengthProperties( numberOfLayers, 0, WLsize );

		m_Calculated = true;

	}

	void CEquivalentBSDFLayer::calculateWavelengthProperties( size_t const t_NumOfLayers,
	                                                          size_t const t_Start, size_t const t_End ) const {
		for ( auto i = t_Start; i < t_End; ++i ) {
			auto curWL = m_CombinedLayerWavelengths[ i ];
			auto& curLayer = *( *m_LayersWL )[ i ];

			for ( auto aSide : EnumSide() ) {
				for ( size_t k = 0; k < t_NumOfLayers; ++k ) {
					m_TotA.at( aSide )->addProperties( k, curWL, *curLayer.getLayerAbsorptances( k + 1, aSide ) );
				}
				for ( auto aProperty : EnumPropertySimple() ) {
					auto curPropertyMatrix = curLayer.getProperty( aSide, aProperty );
					m_Tot.at( std::make_pair( aSide, aProperty ) )->addProperties( curWL, *curPropertyMatrix );
				}
			}
		}
	}

	void CEquivalentBSDFLayer::updateWavelengthLayers(
		const std::shared_ptr< CBSDFLayer >& t_Layer ) const {
		std::shared_ptr< std::vector< std::shared_ptr< CBSDFIntegrator > > > aResults = nullptr;

		aResults = t_Layer->getWavelengthResults();
		size_t size = m_CombinedLayerWavelengths.size();
		for ( size_t i = 0; i < size; ++i ) {
			double curWL = m_CombinedLayerWavelengths[ i ];
			int index = t_Layer->getBandIndex( curWL );
			assert( index > -1 );

			std::shared_ptr< CBSDFIntegrator > currentLayer = ( *aResults )[ size_t( index ) ];

			if ( m_LayersWL->size() <= i ) {
				std::shared_ptr< CEquivalentBSDFLayerSingleBand > aEquivalentLayer =
					std::make_shared< CEquivalentBSDFLayerSingleBand >( currentLayer );

				m_LayersWL->push_back( aEquivalentLayer );
			}
			else {
				std::shared_ptr< CEquivalentBSDFLayerSingleBand > currentEqLayer = ( *m_LayersWL )[ i ];
				currentEqLayer->addLayer( currentLayer );
			}

		}
	}

}
