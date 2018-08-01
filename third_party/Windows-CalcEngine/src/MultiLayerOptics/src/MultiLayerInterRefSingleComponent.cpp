#include <cassert>

#include "MultiLayerInterRefSingleComponent.hpp"
#include "WCESingleLayerOptics.hpp"
#include "EquivalentLayerSingleComponent.hpp"

using namespace FenestrationCommon;
using namespace SingleLayerOptics;

namespace MultiLayerOptics {

	///////////////////////////////////////////////////////////////////////////////////////////////////////////
	//   CSurfaceEnergy
	///////////////////////////////////////////////////////////////////////////////////////////////////////////
	CSurfaceEnergy::CSurfaceEnergy() {
		for ( Side t_Side : EnumSide() ) {
			for ( EnergyFlow t_EnergyFlow : EnumEnergyFlow() ) {
				m_IEnergy[ std::make_pair( t_Side, t_EnergyFlow ) ] = std::make_shared< std::vector< double > >();
			}
		}
	}

	void CSurfaceEnergy::addEnergy( const Side t_Side, const EnergyFlow t_EnergySide, const double t_Value ) {
		m_IEnergy.at( std::make_pair( t_Side, t_EnergySide ) )->push_back( t_Value );
	}

	double CSurfaceEnergy::IEnergy( const size_t Index, const Side t_Side, const EnergyFlow t_EnergyFlow ) {
		return ( *m_IEnergy[ std::make_pair( t_Side, t_EnergyFlow ) ] )[ Index - 1 ];
	}

	///////////////////////////////////////////////////////////////////////////////////////////////////////////
	//   CInterRefSingleComponent
	///////////////////////////////////////////////////////////////////////////////////////////////////////////

	CInterRefSingleComponent::CInterRefSingleComponent( const double t_Tf, const double t_Rf, const double t_Tb,
	                                                    const double t_Rb ) : m_StateCalculated( false ) {
		initialize( t_Tf, t_Rf, t_Tb, t_Rb );
	}

	CInterRefSingleComponent::CInterRefSingleComponent( const std::shared_ptr< const CLayerSingleComponent >& t_Layer ) :
		m_StateCalculated( false ) {
		double Tf = t_Layer->getProperty( Property::T, Side::Front );
		double Rf = t_Layer->getProperty( Property::R, Side::Front );
		double Tb = t_Layer->getProperty( Property::T, Side::Back );
		double Rb = t_Layer->getProperty( Property::R, Side::Back );
		initialize( Tf, Rf, Tb, Rb );
	}

	void CInterRefSingleComponent::addLayer( const double t_Tf, const double t_Rf, const double t_Tb, const double t_Rb,
	                                         const Side t_Side ) {
		std::shared_ptr< CLayerSingleComponent > aLayer = std::make_shared< CLayerSingleComponent >( t_Tf, t_Rf, t_Tb, t_Rb );
		switch ( t_Side ) {
		case Side::Front:
			m_Layers.insert( m_Layers.begin(), aLayer );
			break;
		case Side::Back:
			m_Layers.push_back( aLayer );
			break;
		default:
			assert("Impossible side selection when adding new layer.");
			break;
		}
		m_StateCalculated = false;
	}

	void CInterRefSingleComponent::addLayer( std::shared_ptr< const CLayerSingleComponent > t_Layer, const Side t_Side ) {
		double Tf = t_Layer->getProperty( Property::T, Side::Front );
		double Rf = t_Layer->getProperty( Property::R, Side::Front );
		double Tb = t_Layer->getProperty( Property::T, Side::Back );
		double Rb = t_Layer->getProperty( Property::R, Side::Back );
		addLayer( Tf, Rf, Tb, Rb, t_Side );
	}

	double CInterRefSingleComponent::getEnergyToSurface( const size_t Index, const Side t_Side,
	                                                     const EnergyFlow t_EnergyFlow ) {
		calculateEnergies();
		return m_IEnergy->IEnergy( Index, t_Side, t_EnergyFlow );
	}

	std::shared_ptr< CSurfaceEnergy > CInterRefSingleComponent::getSurfaceEnergy() {
		calculateEnergies();
		return m_IEnergy;
	}

	double CInterRefSingleComponent::getLayerAbsorptance( const size_t Index, const Side t_Side ) {
		// In this context side means energy flow, so we need to convert side into
		// correct energy flow
		EnergyFlow aFlow = getFlowFromSide( t_Side );

		// Even if energy flow comes from one side, it still hits both sides of the layer and
		// this loop calculates energy absorbed at each side
		double absTot = 0;
		for ( Side aSide : EnumSide() ) {
			absTot += m_Layers[ Index - 1 ]->getProperty( Property::Abs, aSide ) *
				getEnergyToSurface( Index, aSide, aFlow );
		}
		return absTot;
	}

	void CInterRefSingleComponent::initialize( const double t_Tf, const double t_Rf,
	                                           const double t_Tb, const double t_Rb ) {
		m_StateCalculated = false;
		m_IEnergy = std::make_shared< CSurfaceEnergy >();
		addLayer( t_Tf, t_Rf, t_Tb, t_Rb );
	}

	void CInterRefSingleComponent::calculateEnergies() {
		if ( !m_StateCalculated ) {
			calculateForwardLayers();
			calculateBackwardLayers();

			for ( size_t i = 0; i <= m_Layers.size(); ++i ) {
				std::shared_ptr< CLayerSingleComponent > aForwardLayer = m_ForwardLayers[ i ];
				std::shared_ptr< CLayerSingleComponent > aBackwardLayer = m_BackwardLayers[ i ];

				double Tf = aForwardLayer->getProperty( Property::T, Side::Front );
				double Tb = aBackwardLayer->getProperty( Property::T, Side::Back );
				double Rf = aBackwardLayer->getProperty( Property::R, Side::Front );
				double Rb = aForwardLayer->getProperty( Property::R, Side::Back );
				double iReflectance = 1 / ( 1 - Rf * Rb );

				if ( i != m_Layers.size() ) {
					m_IEnergy->addEnergy( Side::Front, EnergyFlow::Forward, Tf * iReflectance );
					m_IEnergy->addEnergy( Side::Front, EnergyFlow::Backward, Tb * Rb * iReflectance );
				}

				if ( i != 0 ) {
					m_IEnergy->addEnergy( Side::Back, EnergyFlow::Forward, Tf * Rf * iReflectance );
					m_IEnergy->addEnergy( Side::Back, EnergyFlow::Backward, Tb * iReflectance );
				}
			}

			m_StateCalculated = true;
		}
	}

	void CInterRefSingleComponent::calculateForwardLayers() {
		// Insert exterior environment properties
		std::shared_ptr< CLayerSingleComponent > aLayer = std::make_shared< CLayerSingleComponent >( 1, 0, 1, 0 );
		m_ForwardLayers.push_back( aLayer );

		// First layer just in. No calculation is needed
		aLayer = m_Layers[ 0 ];
		m_ForwardLayers.push_back( aLayer );
		CEquivalentLayerSingleComponent aEqLayer = CEquivalentLayerSingleComponent( *aLayer );
		for ( size_t i = 1; i < m_Layers.size(); ++i ) {
			aEqLayer.addLayer( *m_Layers[ i ] );
			aLayer = aEqLayer.getLayer();
			m_ForwardLayers.push_back( aLayer );
		}
	}

	void CInterRefSingleComponent::calculateBackwardLayers() {
		// Insert interior environment properties
		std::shared_ptr< CLayerSingleComponent > aLayer = std::make_shared< CLayerSingleComponent >( 1, 0, 1, 0 );
		m_BackwardLayers.push_back( aLayer );

		size_t size = m_Layers.size() - 1;
		// Last layer just in. No calculation is needed
		aLayer = m_Layers[ size ];
		m_BackwardLayers.insert( m_BackwardLayers.begin(), aLayer );
		CEquivalentLayerSingleComponent aEqLayer = CEquivalentLayerSingleComponent( *aLayer );
		for ( size_t i = size; i > 0; --i ) {
			aEqLayer.addLayer( *m_Layers[ i - 1 ], Side::Front );
			aLayer = aEqLayer.getLayer();
			m_BackwardLayers.insert( m_BackwardLayers.begin(), aLayer );
		}
	}

}
