#define _USE_MATH_DEFINES
#include <math.h>
#include <cassert>

#include "DirectionalDiffuseBSDFLayer.hpp"
#include "DirectionalDiffuseCell.hpp"
#include "BSDFIntegrator.hpp"
#include "BSDFDirections.hpp"
#include "BeamDirection.hpp"
#include "BSDFPatch.hpp"
#include "WCECommon.hpp"

using namespace std;
using namespace FenestrationCommon;

namespace SingleLayerOptics {

	CDirectionalDiffuseBSDFLayer::CDirectionalDiffuseBSDFLayer( const std::shared_ptr< CDirectionalDiffuseCell >& t_Cell,
	                                                            const std::shared_ptr< const CBSDFHemisphere >& t_Hemisphere ) :
		CBSDFLayer( t_Cell, t_Hemisphere ) {

	}

	std::shared_ptr< CDirectionalDiffuseCell > CDirectionalDiffuseBSDFLayer::cellAsDirectionalDiffuse() const {
		std::shared_ptr< CDirectionalDiffuseCell > aCell = dynamic_pointer_cast< CDirectionalDiffuseCell >( m_Cell );
		assert( aCell != nullptr );
		return aCell;
	}

	void CDirectionalDiffuseBSDFLayer::calcDiffuseDistribution( const Side aSide,
	                                                            const CBeamDirection& t_Direction,
	                                                            const size_t t_DirectionIndex ) {

		std::shared_ptr< CDirectionalDiffuseCell > aCell = cellAsDirectionalDiffuse();

		std::shared_ptr< CSquareMatrix > Tau = m_Results->getMatrix( aSide, PropertySimple::T );
		std::shared_ptr< CSquareMatrix > Rho = m_Results->getMatrix( aSide, PropertySimple::R );

		std::shared_ptr< const CBSDFDirections > jDirections =
			m_BSDFHemisphere->getDirections( BSDFHemisphere::Outgoing );

		size_t size = jDirections->size();

		for ( size_t j = 0; j < size; ++j ) {

			const CBeamDirection jDirection = *( *jDirections )[ j ]->centerPoint();

			double aTau = aCell->T_dir_dif( aSide, t_Direction, jDirection );
			double aRho = aCell->R_dir_dif( aSide, t_Direction, jDirection );

			( *Tau )[ j ][ t_DirectionIndex ] += aTau / M_PI;
			( *Rho )[ j ][ t_DirectionIndex ] += aRho / M_PI;
		}

	}

	void CDirectionalDiffuseBSDFLayer::calcDiffuseDistribution_wv( const Side aSide,
	                                                               const CBeamDirection& t_Direction,
	                                                               const size_t t_DirectionIndex ) {

		std::shared_ptr< CDirectionalDiffuseCell > aCell = cellAsDirectionalDiffuse();

		CBSDFDirections iDirections = *m_BSDFHemisphere->getDirections( BSDFHemisphere::Outgoing );

		size_t size = iDirections.size();

		for ( size_t i = 0; i < size; ++i ) {

			const CBeamDirection iDirection = *iDirections[ i ]->centerPoint();

			std::shared_ptr< std::vector< double > > aTau = aCell->T_dir_dif_band( aSide, t_Direction, iDirection );
			std::shared_ptr< std::vector< double > > Ref = aCell->R_dir_dif_band( aSide, t_Direction, iDirection );

			size_t numWV = aTau->size();
			for ( size_t j = 0; j < numWV; ++j ) {
				std::shared_ptr< CBSDFIntegrator > aResults = nullptr;
				aResults = ( *m_WVResults )[ j ];
				assert( aResults != nullptr );
				std::shared_ptr< CSquareMatrix > Tau = aResults->getMatrix( aSide, PropertySimple::T );
				std::shared_ptr< CSquareMatrix > Rho = aResults->getMatrix( aSide, PropertySimple::R );
				( *Tau )[ i ][ t_DirectionIndex ] += ( *aTau )[ j ] / M_PI;
				( *Rho )[ i ][ t_DirectionIndex ] += ( *Ref )[ j ] / M_PI;
			}

		}

	}

}
