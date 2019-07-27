#include <cassert>
#include <iostream>

#include "HeatFlowBalance.hpp"
#include "BaseLayer.hpp"
#include "BaseIGULayer.hpp"
#include "IGUSolidLayer.hpp"
#include "IGU.hpp"
#include "Environment.hpp"
#include "Surface.hpp"
#include "WCEGases.hpp"
#include "WCECommon.hpp"


using namespace FenestrationCommon;

namespace Tarcog {

	CHeatFlowBalance::CHeatFlowBalance( std::shared_ptr< CIGU > const& t_IGU ) : m_IGU( t_IGU ) {
		m_MatrixA = std::make_shared< CSquareMatrix >( 4 * m_IGU->getNumOfLayers() );
		assert( m_MatrixA != nullptr );
		m_VectorB = std::make_shared< std::vector< double > >();
		assert( m_VectorB != nullptr );
		m_VectorB->resize( 4 * m_IGU->getNumOfLayers() );
		m_LinearSolver = std::make_shared< CLinearSolver >();
	}

	std::vector< double > CHeatFlowBalance::calcBalanceMatrix() {
		std::vector< std::shared_ptr< CIGUSolidLayer > > aSolidLayers = m_IGU->getSolidLayers();
		std::vector< std::shared_ptr< CIGUSolidLayer > >::iterator it;
		int positionCounter = 0;
		m_MatrixA->setZeros();
		for ( size_t i = 0; i < m_VectorB->size(); ++i ) {
			( *m_VectorB )[ i ] = 0;
		}
		for ( it = aSolidLayers.begin(); it != aSolidLayers.end(); ++it ) {
			std::shared_ptr< CBaseLayer > aPreviousLayer = ( *it )->getPreviousLayer();
			std::shared_ptr< CBaseLayer > aNextLayer = ( *it )->getNextLayer();
			buildCell( aPreviousLayer, ( *it ), aNextLayer, positionCounter );
			++positionCounter;
		}
		return m_LinearSolver->solveSystem( *m_MatrixA, *m_VectorB );
	}

	void CHeatFlowBalance::buildCell( std::shared_ptr< CBaseLayer > const& t_Previous,
	                                  std::shared_ptr< CBaseLayer > const& t_Current,
	                                  std::shared_ptr< CBaseLayer > const& t_Next, int const t_Index ) {
		// Routine is used to build matrix "cell" around solid layer.

		// first determine cell size
		int sP = 4 * t_Index;

		// First build base cell
		double hgl = t_Current->getConductionConvectionCoefficient();
		double hgap_prev = t_Previous->getConductionConvectionCoefficient();
		double hgap_next = t_Next->getConductionConvectionCoefficient();
		std::shared_ptr< ISurface > frontSurface = t_Current->getSurface( Side::Front );
		assert( frontSurface != nullptr );
		double emissPowerFront = frontSurface->emissivePowerTerm();
		std::shared_ptr< ISurface > backSurface = t_Current->getSurface( Side::Back );
		assert( backSurface != nullptr );
		double emissPowerBack = backSurface->emissivePowerTerm();
		double qv_prev = t_Previous->getGainFlow();
		double qv_next = t_Next->getGainFlow();
		double solarRadiation = t_Current->getGainFlow();

		// first row
		( *m_MatrixA )[ sP ][ sP ] = hgap_prev + hgl;
		( *m_MatrixA )[ sP ][ sP + 1 ] = 1;
		( *m_MatrixA )[ sP ][ sP + 3 ] = - hgl;
		( *m_VectorB )[ sP ] = solarRadiation / 2 + qv_prev / 2;

		// second row
		( *m_MatrixA )[ sP + 1 ][ sP ] = emissPowerFront;
		( *m_MatrixA )[ sP + 1 ][ sP + 1 ] = -1;

		// third row
		( *m_MatrixA )[ sP + 2 ][ sP + 2 ] = -1;
		( *m_MatrixA )[ sP + 2 ][ sP + 3 ] = emissPowerBack;

		// fourth row
		( *m_MatrixA )[ sP + 3 ][ sP ] = hgl;
		( *m_MatrixA )[ sP + 3 ][ sP + 2 ] = -1;
		( *m_MatrixA )[ sP + 3 ][ sP + 3 ] = -hgap_next - hgl;
		( *m_VectorB )[ sP + 3 ] = -solarRadiation / 2 - qv_next / 2;


		if ( std::dynamic_pointer_cast< CEnvironment >( t_Previous ) == NULL ) {

			// first row
			( *m_MatrixA )[ sP ][ sP - 2 ] = -1;
			( *m_MatrixA )[ sP ][ sP - 1 ] = -hgap_prev;

			// second row
			( *m_MatrixA )[ sP + 1 ][ sP - 2 ] = frontSurface->getReflectance();

			// third row
			( *m_MatrixA )[ sP + 2 ][ sP - 2 ] = frontSurface->getTransmittance();

		}
		else {
			double environmentRadiosity = std::dynamic_pointer_cast< CEnvironment >( t_Previous )->getEnvironmentIR();
			double airTemperature = std::dynamic_pointer_cast< CEnvironment >( t_Previous )->getGasTemperature();

			( *m_VectorB )[ sP ] = ( *m_VectorB )[ sP ] + environmentRadiosity + hgap_prev * airTemperature;
			( *m_VectorB )[ sP + 1 ] = ( *m_VectorB )[ sP + 1 ] - frontSurface->getReflectance() * environmentRadiosity;
			( *m_VectorB )[ sP + 2 ] = ( *m_VectorB )[ sP + 2 ] - frontSurface->getTransmittance() * environmentRadiosity;
		}

		if ( std::dynamic_pointer_cast< CEnvironment >( t_Next ) == NULL ) {

			// second row
			( *m_MatrixA )[ sP + 1 ][ sP + 5 ] = backSurface->getTransmittance();

			// third row
			( *m_MatrixA )[ sP + 2 ][ sP + 5 ] = backSurface->getReflectance();

			// fourth row
			( *m_MatrixA )[ sP + 3 ][ sP + 4 ] = hgap_next;
			( *m_MatrixA )[ sP + 3 ][ sP + 5 ] = 1;

		}
		else {
			double environmentRadiosity = std::dynamic_pointer_cast< CEnvironment >( t_Next )->getEnvironmentIR();
			double airTemperature = std::dynamic_pointer_cast< CEnvironment >( t_Next )->getGasTemperature();

			( *m_VectorB )[ sP + 1 ] = ( *m_VectorB )[ sP + 1 ] - backSurface->getTransmittance() * environmentRadiosity;
			( *m_VectorB )[ sP + 2 ] = ( *m_VectorB )[ sP + 2 ] - backSurface->getReflectance() * environmentRadiosity;
			( *m_VectorB )[ sP + 3 ] = ( *m_VectorB )[ sP + 3 ] - environmentRadiosity - hgap_next * airTemperature;
		}


	}

}
