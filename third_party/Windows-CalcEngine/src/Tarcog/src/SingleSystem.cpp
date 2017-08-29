#include <memory>
#include <vector>
#include <stdexcept>
#include <cassert>

#include "SingleSystem.hpp"
#include "BaseLayer.hpp"
#include "BaseIGULayer.hpp"
#include "IGUSolidLayer.hpp"
#include "IGU.hpp"
#include "OutdoorEnvironment.hpp"
#include "IndoorEnvironment.hpp"
#include "Surface.hpp"
#include "NonLinearSolver.hpp"
#include "WCECommon.hpp"


using namespace FenestrationCommon;

namespace Tarcog {

	CSingleSystem::CSingleSystem( std::shared_ptr< CIGU > const& t_IGU,
	                              std::shared_ptr< CEnvironment > const& t_Indoor, std::shared_ptr< CEnvironment > const& t_Outdoor ) :
		m_IGU( t_IGU ) {

		m_Environment[ Environment::Indoor ] = t_Indoor;
		m_Environment[ Environment::Outdoor ] = t_Outdoor;

		if ( t_IGU == nullptr ) {
			throw std::runtime_error( "IGU has not been assigned to the system. Null value passed." );
		}

		if ( t_Indoor == nullptr ) {
			throw std::runtime_error( "Indoor environment has not been assigned to the system. Null value passed." );
		}

		if ( t_Outdoor == nullptr ) {
			throw std::runtime_error( "Outdoor environment has not been assigned to the system. Null value passed." );
		}

		auto aIndoorLayer = m_IGU->getLayer( Environment::Indoor );
		auto aIndoor = m_Environment.at( Environment::Indoor );
		aIndoor->connectToIGULayer( aIndoorLayer );
		aIndoor->setTilt( m_IGU->getTilt() );
		aIndoor->setWidth( m_IGU->getWidth() );
		aIndoor->setHeight( m_IGU->getHeight() );

		auto aOutdoorLayer = m_IGU->getLayer( Environment::Outdoor );
		auto aOutdoor = m_Environment.at( Environment::Outdoor );
		aOutdoor->connectToIGULayer( aOutdoorLayer );
		aOutdoor->setTilt( m_IGU->getTilt() );
		aOutdoor->setWidth( m_IGU->getWidth() );
		aOutdoor->setHeight( m_IGU->getHeight() );

		auto solarRadiation = t_Outdoor->getDirectSolarRadiation();
		m_IGU->setSolarRadiation( solarRadiation );

		initializeStartValues();

		m_NonLinearSolver = std::make_shared< CNonLinearSolver >( m_IGU );
	}

	CSingleSystem::CSingleSystem( CSingleSystem const& t_SingleSystem ) {
		m_IGU = std::make_shared< CIGU >( *t_SingleSystem.m_IGU );

		m_Environment[ Environment::Indoor ] = t_SingleSystem.m_Environment.at( Environment::Indoor )->cloneEnvironment();
		auto aLastLayer = m_IGU->getLayer( Environment::Indoor );
		m_Environment.at( Environment::Indoor )->connectToIGULayer( aLastLayer );

		m_Environment[ Environment::Outdoor ] = t_SingleSystem.m_Environment.at( Environment::Outdoor )->cloneEnvironment();
		auto aFirstLayer = m_IGU->getLayer( Environment::Outdoor );
		m_Environment.at( Environment::Outdoor )->connectToIGULayer( aFirstLayer );

		initializeStartValues();

		m_NonLinearSolver = std::make_shared< CNonLinearSolver >( m_IGU );
	}

	std::vector< std::shared_ptr< CIGUSolidLayer > > CSingleSystem::getSolidLayers() const {
		return m_IGU->getSolidLayers();
	}

	std::vector< std::shared_ptr< CIGUGapLayer > > CSingleSystem::getGapLayers() const {
		return m_IGU->getGapLayers();
	}

	std::shared_ptr< std::vector< double > > CSingleSystem::getTemperatures() const {
		return m_IGU->getTemperatures();
	}

	std::shared_ptr< std::vector< double > > CSingleSystem::getRadiosities() const {
		return m_IGU->getRadiosities();
	}

	std::shared_ptr< std::vector< double > > CSingleSystem::getMaxDeflections() const {
		return m_IGU->getMaxDeflections();
	}

	std::shared_ptr< std::vector< double > > CSingleSystem::getMeanDeflections() const {
		return m_IGU->getMeanDeflections();
	}

	std::shared_ptr< CSingleSystem > CSingleSystem::clone() const {
		return std::make_shared< CSingleSystem >( *this );
	}

	double CSingleSystem::getHeatFlow( Environment const t_Environment ) const {
		return m_Environment.at( t_Environment )->getHeatFlow();
	}

	double CSingleSystem::getConvectiveHeatFlow( Environment const t_Environment ) const {
		return m_Environment.at( t_Environment )->getConvectionConductionFlow();
	}

	double CSingleSystem::getRadiationHeatFlow( Environment const t_Environment ) const {
		return m_Environment.at( t_Environment )->getRadiationFlow();
	}

	double CSingleSystem::getHc( Environment const t_Environment ) const {
		return m_Environment.at( t_Environment )->getHc();
	}

	double CSingleSystem::getAirTemperature( Environment const t_Environment ) const {
		return m_Environment.at( t_Environment )->getAirTemperature();
	}

	double CSingleSystem::getVentilationFlow( Environment const t_Environment ) const {
		return m_IGU->getVentilationFlow( t_Environment );
	}

	double CSingleSystem::getUValue() const {
		double interiorAir = m_Environment.at( Environment::Indoor )->getAmbientTemperature();
		double outdoorAir = m_Environment.at( Environment::Outdoor )->getAmbientTemperature();
		return getHeatFlow( Environment::Indoor ) / ( interiorAir - outdoorAir );
	}

	void CSingleSystem::setTolerance( double const t_Tolerance ) const {
		assert( m_NonLinearSolver != nullptr );
		m_NonLinearSolver->setTolerance( t_Tolerance );
	}

	size_t CSingleSystem::getNumberOfIterations() const {
		assert( m_NonLinearSolver != nullptr );
		return m_NonLinearSolver->getNumOfIterations();
	}

	double CSingleSystem::solutionTolarance() const {
		assert( m_NonLinearSolver != nullptr );
		return m_NonLinearSolver->solutionTolerance();
	}

	bool CSingleSystem::isToleranceAchieved() const {
		assert( m_NonLinearSolver != nullptr );
		return m_NonLinearSolver->isToleranceAchieved();
	}

	void CSingleSystem::solve() const {
		assert( m_NonLinearSolver != nullptr );
		m_NonLinearSolver->solve();
	}

	void CSingleSystem::initializeStartValues() {
		auto const startX = 0.001;
		auto thickness = m_IGU->getThickness() + startX + 0.01;
		auto tOut = m_Environment.at( Environment::Outdoor )->getGasTemperature();
		auto tInd = m_Environment.at( Environment::Indoor )->getGasTemperature();

		auto deltaTemp = ( tInd - tOut ) / thickness;

		auto aLayers = m_IGU->getLayers();

		auto aLayer = aLayers.front();
		auto currentXPosition = startX;
		auto aSurface = aLayer->getSurface( Side::Front );
		auto curTemp = tOut + currentXPosition * deltaTemp;

		aSurface->initializeStart( curTemp );

		for ( auto layer : aLayers ) {
			currentXPosition += layer->getThickness();
			curTemp = tOut + currentXPosition * deltaTemp;
			aSurface = layer->getSurface( Side::Back );
			aSurface->initializeStart( curTemp );
		}
	}

	void CSingleSystem::setInitialGuess( std::vector< double > const& t_Temperatures ) const {
		m_IGU->setInitialGuess( t_Temperatures );
	}

	void CSingleSystem::setSolarRadiation( double const t_SolarRadiation ) {
		std::dynamic_pointer_cast< COutdoorEnvironment >( m_Environment.at( Environment::Outdoor ) )->
			setSolarRadiation( t_SolarRadiation );
		m_IGU->setSolarRadiation( t_SolarRadiation );
	}

	double CSingleSystem::getSolarRadiation() const {
		return std::dynamic_pointer_cast< COutdoorEnvironment >( m_Environment.at( Environment::Outdoor ) )->
			getSolarRadiation();
	}

}
