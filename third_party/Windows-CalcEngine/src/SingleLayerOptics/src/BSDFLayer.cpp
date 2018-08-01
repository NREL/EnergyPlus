#include "BSDFLayer.hpp"
#include "BaseCell.hpp"
#include "BSDFDirections.hpp"
#include "BSDFIntegrator.hpp"
#include "MaterialDescription.hpp"
#include "BSDFPatch.hpp"
#include "WCECommon.hpp"
#include "BeamDirection.hpp"

using namespace FenestrationCommon;

namespace SingleLayerOptics {

	CBSDFLayer::CBSDFLayer( const std::shared_ptr< CBaseCell >& t_Cell,
	                        const std::shared_ptr< const CBSDFHemisphere >& t_Hemisphere ) :
		m_BSDFHemisphere( t_Hemisphere ), m_Cell( t_Cell ), m_Calculated( false ), m_CalculatedWV( false ) {

		// TODO: Maybe to refactor results to incoming and outgoing if not affecting speed.
		// This is not necessary before axisymmetry is introduced
		m_Results = std::make_shared< CBSDFIntegrator >( m_BSDFHemisphere->getDirections( BSDFHemisphere::Incoming ) );
	}

	void CBSDFLayer::setSourceData( std::shared_ptr< CSeries > t_SourceData ) {
		m_Cell->setSourceData( t_SourceData );
		m_Calculated = false;
		m_CalculatedWV = false;
	}

	std::shared_ptr< const CBSDFDirections > CBSDFLayer::getDirections( const BSDFHemisphere t_Side ) const {
		return m_BSDFHemisphere->getDirections( t_Side );
	}

	std::shared_ptr< CBSDFIntegrator > CBSDFLayer::getResults() {
		if ( !m_Calculated ) {
			calculate();
			m_Calculated = true;
		}
		return m_Results;
	}

	std::shared_ptr< BSDF_Results > CBSDFLayer::getWavelengthResults() {
		if ( !m_CalculatedWV ) {
			calculate_wv();
			m_CalculatedWV = true;
		}
		return m_WVResults;
	}

	int CBSDFLayer::getBandIndex( const double t_Wavelength ) {
		return m_Cell->getBandIndex( t_Wavelength );
	}

	std::vector< double > CBSDFLayer::getBandWavelengths() const {
		return m_Cell->getBandWavelengths();
	}

	void CBSDFLayer::calc_dir_dir() {
		for ( Side t_Side : EnumSide() ) {
			CBSDFDirections aDirections = *m_BSDFHemisphere->getDirections( BSDFHemisphere::Incoming );
			size_t size = aDirections.size();
			std::shared_ptr< CSquareMatrix > Tau = std::make_shared< CSquareMatrix >( size );
			std::shared_ptr< CSquareMatrix > Rho = std::make_shared< CSquareMatrix >( size );
			for ( size_t i = 0; i < size; ++i ) {
				const CBeamDirection aDirection = *aDirections[ i ]->centerPoint();
				double Lambda = aDirections[ i ]->lambda();

				double aTau = m_Cell->T_dir_dir( t_Side, aDirection );
				double aRho = m_Cell->R_dir_dir( t_Side, aDirection );

				( *Tau )[ i ][ i ] += aTau / Lambda;
				( *Rho )[ i ][ i ] += aRho / Lambda;
			}
			m_Results->setResultMatrices( Tau, Rho, t_Side );
		}
	}

	void CBSDFLayer::calc_dir_dir_wv() {
		for ( Side aSide : EnumSide() ) {
			std::shared_ptr< const CBSDFDirections > aDirections =
				m_BSDFHemisphere->getDirections( BSDFHemisphere::Incoming );
			size_t size = aDirections->size();
			for ( size_t i = 0; i < size; ++i ) {
				const CBeamDirection aDirection = *( *aDirections )[ i ]->centerPoint();
				std::vector< double > aTau = m_Cell->T_dir_dir_band( aSide, aDirection );
				std::vector< double > aRho = m_Cell->R_dir_dir_band( aSide, aDirection );
				double Lambda = ( *aDirections )[ i ]->lambda();
				std::shared_ptr< CSquareMatrix > Tau = nullptr;
				std::shared_ptr< CSquareMatrix > Rho = nullptr;
				size_t numWV = aTau.size();
				for ( size_t j = 0; j < numWV; ++j ) {
					CBSDFIntegrator aResults = *( *m_WVResults )[ j ];
					Tau = aResults.getMatrix( aSide, PropertySimple::T );
					Rho = aResults.getMatrix( aSide, PropertySimple::R );
					( *Tau )[ i ][ i ] += aTau[ j ] / Lambda;
					( *Rho )[ i ][ i ] += aRho[ j ] / Lambda;
				}
			}
		}
	}

	void CBSDFLayer::calc_dir_dif() {
		for ( Side aSide : EnumSide() ) {

			CBSDFDirections aDirections = *m_BSDFHemisphere->getDirections( BSDFHemisphere::Incoming );

			size_t size = aDirections.size();
			for ( size_t i = 0; i < size; ++i ) {
				const CBeamDirection aDirection = *aDirections[ i ]->centerPoint();
				calcDiffuseDistribution( aSide, aDirection, i );
			}
		}
	}

	void CBSDFLayer::calc_dir_dif_wv() {
		for ( Side aSide : EnumSide() ) {

			CBSDFDirections aDirections = *m_BSDFHemisphere->getDirections( BSDFHemisphere::Incoming );

			size_t size = aDirections.size();
			for ( size_t i = 0; i < size; ++i ) {
				const CBeamDirection aDirection = *aDirections[ i ]->centerPoint();
				calcDiffuseDistribution_wv( aSide, aDirection, i );
			}
		}
	}

	void CBSDFLayer::fillWLResultsFromMaterialCell() {
		m_WVResults = std::make_shared< std::vector< std::shared_ptr< CBSDFIntegrator > > >();
		size_t size = m_Cell->getBandSize();
		for ( size_t i = 0; i < size; ++i ) {
			std::shared_ptr< CBSDFIntegrator > aResults =
				std::make_shared< CBSDFIntegrator >( m_BSDFHemisphere->getDirections( BSDFHemisphere::Incoming ) );
			m_WVResults->push_back( aResults );
		}
	}

	void CBSDFLayer::calculate() {
		fillWLResultsFromMaterialCell();
		calc_dir_dir();
		calc_dir_dif();
	}

	void CBSDFLayer::calculate_wv() {
		fillWLResultsFromMaterialCell();
		calc_dir_dir_wv();
		calc_dir_dif_wv();
	}

}
