#include <cassert>
#include <stdexcept>

#include "MultiLayerInterRef.hpp"
#include "WCESingleLayerOptics.hpp"
#include "WCECommon.hpp"
#include "EquivalentScatteringLayer.hpp"
#include "MultiLayerInterRefSingleComponent.hpp"

using namespace FenestrationCommon;
using namespace SingleLayerOptics;

namespace MultiLayerOptics {

	CInterRef::CInterRef( const std::shared_ptr< CScatteringLayer >& t_Layer, const double t_Theta, const double t_Phi ) :
		m_StateCalculated( false ), m_Theta( t_Theta ), m_Phi( t_Phi ) {
		m_Layers.push_back( t_Layer );
		for ( Scattering aScattering : EnumScattering() ) {
			m_Energy[ aScattering ] = std::make_shared< CSurfaceEnergy >();
		}
		for ( Side aSide : EnumSide() ) {
			m_StackedLayers[ aSide ] = std::make_shared< CLayer_List >();
		}
		std::shared_ptr< CLayerSingleComponent > aLayer = t_Layer->getLayer( Scattering::DirectDirect, t_Theta, t_Phi );
		m_DirectComponent = std::make_shared< CInterRefSingleComponent >( aLayer );
		aLayer = t_Layer->getLayer( Scattering::DiffuseDiffuse, t_Theta, t_Phi );
		m_DiffuseComponent = std::make_shared< CInterRefSingleComponent >( aLayer );

		for ( Side aSide : EnumSide() ) {
			m_Abs[ std::make_pair( aSide, ScatteringSimple::Diffuse ) ] = std::make_shared< std::vector< double > >();
			m_Abs[ std::make_pair( aSide, ScatteringSimple::Direct ) ] = std::make_shared< std::vector< double > >();
		}
	}

	void CInterRef::addLayer( const std::shared_ptr< CScatteringLayer >& t_Layer, const Side t_Side,
	                          const double t_Theta, const double t_Phi ) {
		switch ( t_Side ) {
		case Side::Front:
			m_Layers.insert( m_Layers.begin(), t_Layer );
			break;
		case Side::Back:
			m_Layers.push_back( t_Layer );
			break;
		default:
			assert( "Impossible side selection when adding new layer." );
			break;
		}

		// addition for pure components (direct and diffuse)
		std::shared_ptr< CLayerSingleComponent > aLayer = t_Layer->getLayer( Scattering::DirectDirect, t_Theta, t_Phi );
		m_DirectComponent->addLayer( aLayer, t_Side );
		aLayer = t_Layer->getLayer( Scattering::DiffuseDiffuse, t_Theta, t_Phi );
		m_DiffuseComponent->addLayer( aLayer, t_Side );

		m_StateCalculated = false;
	}

	double CInterRef::getAbsorptance( const size_t Index, Side t_Side, ScatteringSimple t_Scattering,
	                                  const double t_Theta, const double t_Phi ) {
		calculateEnergies( t_Theta, t_Phi );
		std::shared_ptr< std::vector< double > > aVector = m_Abs.at( std::make_pair( t_Side, t_Scattering ) );
		size_t vecSize = aVector->size();
		if ( vecSize < Index ) {
			throw std::range_error( "Requested layer index is out of range." );
		}
		return ( *aVector )[ Index - 1 ];
	}

	double CInterRef::getEnergyToSurface( const size_t Index, const Side t_SurfaceSide,
	                                      const EnergyFlow t_EnergyFlow, const Scattering t_Scattering, const double t_Theta, const double t_Phi ) {
		calculateEnergies( t_Theta, t_Phi );
		std::shared_ptr< CSurfaceEnergy > aEnergy = m_Energy.at( t_Scattering );
		return aEnergy->IEnergy( Index, t_SurfaceSide, t_EnergyFlow );
	}

	size_t CInterRef::size() const {
		return m_Layers.size();
	}

	void CInterRef::calculateEnergies( const double t_Theta, const double t_Phi ) {
		if ( ( !m_StateCalculated ) || ( t_Theta != m_Theta ) || ( t_Phi != m_Phi ) ) {
			createForwardLayers( t_Theta, t_Phi );
			createBackwardLayers( t_Theta, t_Phi );

			m_Energy[ Scattering::DirectDirect ] = m_DirectComponent->getSurfaceEnergy();
			m_Energy[ Scattering::DiffuseDiffuse ] = m_DiffuseComponent->getSurfaceEnergy();
			m_Energy[ Scattering::DirectDiffuse ] = calcDirectToDiffuseComponent( t_Theta, t_Phi );

			calculateAbsroptances( t_Theta, t_Phi );

			m_StateCalculated = true;
			m_Theta = t_Theta;
			m_Phi = t_Phi;

		}
	}

	void CInterRef::createForwardLayers( const double t_Theta, const double t_Phi ) {
		std::shared_ptr< CLayer_List > aLayers = m_StackedLayers.at( Side::Front );

		// Insert exterior environment first
		std::shared_ptr< CScatteringSurface > aFront = std::make_shared< CScatteringSurface >( 1, 0, 0, 0, 1, 0 );
		std::shared_ptr< CScatteringSurface > aBack = std::make_shared< CScatteringSurface >( 1, 0, 0, 0, 1, 0 );
		std::shared_ptr< CScatteringLayer > exterior = std::make_shared< CScatteringLayer >( aFront, aBack );
		aLayers->push_back( exterior );

		std::shared_ptr< CScatteringLayer > aLayer = m_Layers[ 0 ];
		aLayers->push_back( aLayer );
		CEquivalentScatteringLayer aEqLayer = CEquivalentScatteringLayer( *aLayer, t_Theta, t_Phi );
		for ( size_t i = 1; i < m_Layers.size(); ++i ) {
			aEqLayer.addLayer( *m_Layers[ i ], Side::Back, t_Theta, t_Phi );
			aLayer = aEqLayer.getLayer();
			aLayers->push_back( aLayer );
		}

		aLayers->push_back( exterior );
	}

	void CInterRef::createBackwardLayers( const double t_Theta, const double t_Phi ) {
		std::shared_ptr< CLayer_List > aLayers = m_StackedLayers.at( Side::Back );

		// Insert interior environment
		std::shared_ptr< CScatteringSurface > aFront = std::make_shared< CScatteringSurface >( 1, 0, 0, 0, 1, 0 );
		std::shared_ptr< CScatteringSurface > aBack = std::make_shared< CScatteringSurface >( 1, 0, 0, 0, 1, 0 );
		std::shared_ptr< CScatteringLayer > exterior = std::make_shared< CScatteringLayer >( aFront, aBack );
		aLayers->push_back( exterior );

		size_t size = m_Layers.size() - 1;
		// Last layer just in
		std::shared_ptr< CScatteringLayer > aLayer = m_Layers[ size ];
		aLayers->insert( aLayers->begin(), aLayer );
		CEquivalentScatteringLayer aEqLayer = CEquivalentScatteringLayer( *aLayer, t_Theta, t_Phi );
		for ( size_t i = size; i > 0; --i ) {
			aEqLayer.addLayer( *m_Layers[ i - 1 ], Side::Front, t_Theta, t_Phi );
			aLayer = aEqLayer.getLayer();
			aLayers->insert( aLayers->begin(), aLayer );
		}
		aLayers->insert( aLayers->begin(), exterior );
	}

	std::shared_ptr< CSurfaceEnergy > CInterRef::calcDiffuseEnergy( const double t_Theta, const double t_Phi ) {
		//Sum of previous two components. Total diffuse energy that gets off the surfaces.
		std::shared_ptr< CSurfaceEnergy > diffSum = std::make_shared< CSurfaceEnergy >();

		for ( EnergyFlow aEnergyFlow : EnumEnergyFlow() ) {
			for ( size_t i = 1; i <= m_Layers.size(); ++i ) { // Layer indexing goes from one
				for ( Side aSide : EnumSide() ) {
					Side oppSide = oppositeSide( aSide );
					// Calculate diffuse energy from direct exterior/interior beam
					double beamEnergy = 0;

					std::shared_ptr< CScatteringLayer > curLayer = ( *m_StackedLayers.at( oppSide ) )[ i ];

					if ( ( aSide == Side::Front && aEnergyFlow == EnergyFlow::Backward ) ||
						( aSide == Side::Back && aEnergyFlow == EnergyFlow::Forward ) ) {
						beamEnergy = curLayer->getPropertySimple( PropertySimple::T, oppSide,
						                                          Scattering::DirectDiffuse, t_Theta, t_Phi );
					}

					// Energy that gets converted to diffuse from beam that comes from interreflections in 
					// the gap or interior/exterior environments
					double R = curLayer->getPropertySimple( PropertySimple::R, aSide,
					                                        Scattering::DirectDiffuse, t_Theta, t_Phi );
					double intEnergy = R * m_Energy.at( Scattering::DirectDirect )->IEnergy( i, aSide, aEnergyFlow );
					diffSum->addEnergy( aSide, aEnergyFlow, beamEnergy + intEnergy );
				}
			}
		}

		return diffSum;
	}

	std::shared_ptr< CSurfaceEnergy > CInterRef::calcDirectToDiffuseComponent(
		const double t_Theta, const double t_Phi ) {
		// Gets total diffuse components that is getting off (leaving) every surface.
		// Keep in mind that diffuse componet here only comes from scattering direct beam.
		std::shared_ptr< CSurfaceEnergy > diffSum = calcDiffuseEnergy( t_Theta, t_Phi );

		// Now need to calculate interreflections of total diffuse components that are leaving
		// every surface and calculate total diffuse component that is incoming to every surface.
		std::shared_ptr< CSurfaceEnergy > aScatter = std::make_shared< CSurfaceEnergy >();

		// Calculate total energy scatterred from beam to diffuse
		for ( EnergyFlow aEnergyFlow : EnumEnergyFlow() ) {
			// In this case numbering goes through gas environments (gaps, interior and exterior)
			// becase we want to keep interreflectance calculations together
			for ( size_t i = 0; i <= m_Layers.size(); ++i ) {
				std::shared_ptr< CScatteringLayer > fwdLayer = ( *m_StackedLayers.at( Side::Front ) )[ i ];
				std::shared_ptr< CScatteringLayer > bkwLayer = ( *m_StackedLayers.at( Side::Back ) )[ i + 1 ];
				double Ib = 0;
				if ( i != 0 ) {
					Ib = diffSum->IEnergy( i, Side::Back, aEnergyFlow );
				}
				double If = 0;
				if ( i != m_Layers.size() ) {
					If = diffSum->IEnergy( i + 1, Side::Front, aEnergyFlow );
				}
				double Rf_bkw = bkwLayer->getPropertySimple( PropertySimple::R, Side::Front, Scattering::DiffuseDiffuse, t_Theta, t_Phi );
				double Rb_fwd = fwdLayer->getPropertySimple( PropertySimple::R, Side::Back, Scattering::DiffuseDiffuse, t_Theta, t_Phi );
				double interRef = 1 / ( 1 - Rf_bkw * Rb_fwd );
				double Ib_tot = ( Ib * Rf_bkw + If ) * interRef;
				double If_tot = ( Ib + Rb_fwd * If ) * interRef;
				if ( i != 0 ) {
					aScatter->addEnergy( Side::Back, aEnergyFlow, Ib_tot );
				}
				if ( i != m_Layers.size() ) {
					aScatter->addEnergy( Side::Front, aEnergyFlow, If_tot );
				}
			}
		}

		return aScatter;
	}

	void CInterRef::calculateAbsroptances( const double t_Theta, const double t_Phi ) {
		for ( size_t i = 0; i < m_Layers.size(); ++i ) {
			for ( EnergyFlow aEnergyFlow : EnumEnergyFlow() ) {
				double EnergyDirect = 0;
				double EnergyDiffuse = 0;
				for ( Side aSide : EnumSide() ) {
					double Adir = m_Layers[ i ]->getAbsorptance( aSide, ScatteringSimple::Direct, t_Theta, t_Phi );
					EnergyDirect += Adir * m_Energy[ Scattering::DirectDirect ]->IEnergy( i + 1, aSide, aEnergyFlow );
					double Adif = m_Layers[ i ]->getAbsorptance( aSide, ScatteringSimple::Diffuse, t_Theta, t_Phi );
					EnergyDirect += Adif * m_Energy[ Scattering::DirectDiffuse ]->IEnergy( i + 1, aSide, aEnergyFlow );
					EnergyDiffuse += Adif * m_Energy[ Scattering::DiffuseDiffuse ]->IEnergy( i + 1, aSide, aEnergyFlow );
				}
				// Note that front and back absorptances are actually reffereing to forward and backward
				// energy flows. That is why we need this conversion.
				Side flowSide = getSideFromFlow( aEnergyFlow );
				m_Abs.at( std::make_pair( flowSide, ScatteringSimple::Direct ) )->push_back( EnergyDirect );
				m_Abs.at( std::make_pair( flowSide, ScatteringSimple::Diffuse ) )->push_back( EnergyDiffuse );
			}
		}
	}

}
