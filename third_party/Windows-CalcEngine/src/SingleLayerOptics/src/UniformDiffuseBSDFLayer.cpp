#define _USE_MATH_DEFINES
#include <math.h>
#include <cassert>

#include "UniformDiffuseBSDFLayer.hpp"
#include "UniformDiffuseCell.hpp"
#include "BSDFIntegrator.hpp"
#include "BSDFDirections.hpp"
#include "WCECommon.hpp"
#include "BeamDirection.hpp"

using namespace FenestrationCommon;

namespace SingleLayerOptics {

	CUniformDiffuseBSDFLayer::CUniformDiffuseBSDFLayer( const std::shared_ptr< CUniformDiffuseCell >& t_Cell,
	                                                    const std::shared_ptr< const CBSDFHemisphere >& t_Hemisphere ) :
		CBSDFLayer( t_Cell, t_Hemisphere ) {

	}

	std::shared_ptr< CUniformDiffuseCell > CUniformDiffuseBSDFLayer::cellAsUniformDiffuse() const {
		std::shared_ptr< CUniformDiffuseCell > aCell = std::dynamic_pointer_cast< CUniformDiffuseCell >( m_Cell );
		assert( aCell != nullptr );
		return aCell;
	}

	void CUniformDiffuseBSDFLayer::calcDiffuseDistribution( const Side aSide,
	                                                        const CBeamDirection& t_Direction,
	                                                        const size_t t_DirectionIndex ) {

		std::shared_ptr< CUniformDiffuseCell > aCell = cellAsUniformDiffuse();

		std::shared_ptr< CSquareMatrix > Tau = m_Results->getMatrix( aSide, PropertySimple::T );
		std::shared_ptr< CSquareMatrix > Rho = m_Results->getMatrix( aSide, PropertySimple::R );

		double aTau = aCell->T_dir_dif( aSide, t_Direction );
		double Ref = aCell->R_dir_dif( aSide, t_Direction );

		std::shared_ptr< const CBSDFDirections > aDirections =
			m_BSDFHemisphere->getDirections( BSDFHemisphere::Incoming );
		size_t size = aDirections->size();

		for ( size_t j = 0; j < size; ++j ) {
			( *Tau )[ j ][ t_DirectionIndex ] += aTau / M_PI;
			( *Rho )[ j ][ t_DirectionIndex ] += Ref / M_PI;
		}

	}

	void CUniformDiffuseBSDFLayer::calcDiffuseDistribution_wv( const Side aSide,
	                                                           const CBeamDirection& t_Direction,
	                                                           const size_t t_DirectionIndex ) {

		std::shared_ptr< CUniformDiffuseCell > aCell = cellAsUniformDiffuse();

		std::vector< double > aTau = aCell->T_dir_dif_band( aSide, t_Direction );
		std::vector< double > Ref = aCell->R_dir_dif_band( aSide, t_Direction );

		std::shared_ptr< const CBSDFDirections > aDirections =
			m_BSDFHemisphere->getDirections( BSDFHemisphere::Incoming );
		size_t size = aDirections->size();

		for ( size_t i = 0; i < size; ++i ) {
			size_t numWV = aTau.size();
			for ( size_t j = 0; j < numWV; ++j ) {
				std::shared_ptr< CBSDFIntegrator > aResults = nullptr;
				aResults = ( *m_WVResults )[ j ];
				assert( aResults != nullptr );
				std::shared_ptr< CSquareMatrix > Tau = aResults->getMatrix( aSide, PropertySimple::T );
				std::shared_ptr< CSquareMatrix > Rho = aResults->getMatrix( aSide, PropertySimple::R );
				( *Tau )[ i ][ t_DirectionIndex ] += aTau[ j ] / M_PI;
				( *Rho )[ i ][ t_DirectionIndex ] += Ref[ j ] / M_PI;
			}
		}

	}

}
