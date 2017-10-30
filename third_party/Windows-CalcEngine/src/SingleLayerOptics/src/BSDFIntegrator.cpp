#include "BSDFIntegrator.hpp"
#include "BSDFDirections.hpp"
#include "BSDFPatch.hpp"
#include "WCECommon.hpp"

using namespace std;
using namespace FenestrationCommon;

namespace SingleLayerOptics {

	CBSDFIntegrator::CBSDFIntegrator( const std::shared_ptr< const CBSDFIntegrator >& t_Integrator ) :
			m_Directions( t_Integrator->m_Directions ), m_DimMatrices( m_Directions->size() ),
			m_HemisphericalCalculated( false ), m_DiffuseDiffuseCalculated( false ) {

		for ( auto t_Side : EnumSide() ) {
			for ( auto t_Property : EnumPropertySimple() ) {
				m_Matrix[ make_pair( t_Side, t_Property ) ] = make_shared< CSquareMatrix >( m_DimMatrices );
				m_Hem[ make_pair( t_Side, t_Property ) ] = make_shared< std::vector< double > >( m_DimMatrices );
			}
		}
	}

	CBSDFIntegrator::CBSDFIntegrator( const std::shared_ptr< const CBSDFDirections >& t_Directions ) :
		m_Directions( t_Directions ), m_DimMatrices( m_Directions->size() ),
		m_HemisphericalCalculated( false ), m_DiffuseDiffuseCalculated( false )
	{

		for ( auto t_Side : EnumSide() ) {
			for ( auto t_Property : EnumPropertySimple() ) {
				m_Matrix[ make_pair( t_Side, t_Property ) ] = make_shared< CSquareMatrix >( m_DimMatrices );
				m_Hem[ make_pair( t_Side, t_Property ) ] = make_shared< std::vector< double > >( m_DimMatrices );
			}
		}
	}

	double CBSDFIntegrator::DiffDiff( const Side t_Side, const PropertySimple t_Property ) {
		calcDiffuseDiffuse();
		return m_MapDiffDiff.at( t_Side, t_Property );
	}

	std::shared_ptr< CSquareMatrix > CBSDFIntegrator::getMatrix( const Side t_Side,
	                                                        const PropertySimple t_Property ) const {
		return m_Matrix.at( make_pair( t_Side, t_Property ) );
	}

	void CBSDFIntegrator::setResultMatrices( const std::shared_ptr< CSquareMatrix >& t_Tau,
	                                         const std::shared_ptr< CSquareMatrix >& t_Rho, Side t_Side ) {
		m_Matrix[ make_pair( t_Side, PropertySimple::T ) ] = t_Tau;
		m_Matrix[ make_pair( t_Side, PropertySimple::R ) ] = t_Rho;
	}

	double CBSDFIntegrator::DirDir( const Side t_Side, const PropertySimple t_Property,
	                                const double t_Theta, const double t_Phi ) {
		size_t index = m_Directions->getNearestBeamIndex( t_Theta, t_Phi );
		double lambda = ( *m_Directions->lambdaVector() )[ index ];
		double tau = ( *getMatrix( t_Side, t_Property ) )[ index ][ index ];
		return tau * lambda;
	}

	double CBSDFIntegrator::DirDir( const Side t_Side, const PropertySimple t_Property,
	                                const size_t Index ) {
		double lambda = ( *m_Directions->lambdaVector() )[ Index ];
		double tau = ( *getMatrix( t_Side, t_Property ) )[ Index ][ Index ];
		return tau * lambda;
	}

	std::shared_ptr< std::vector< double > > CBSDFIntegrator::DirHem( const FenestrationCommon::Side t_Side,
	                                                        const FenestrationCommon::PropertySimple t_Property ) {
		calcHemispherical();
		return m_Hem.at( make_pair( t_Side, t_Property ) );
	}

	std::shared_ptr< std::vector< double > > CBSDFIntegrator::Abs( Side t_Side ) {
		calcHemispherical();
		return m_Abs.at( t_Side );
	}

	double CBSDFIntegrator::DirHem( const Side t_Side, const PropertySimple t_Property,
	                                const double t_Theta, const double t_Phi ) {
		size_t index = m_Directions->getNearestBeamIndex( t_Theta, t_Phi );
		return ( *DirHem( t_Side, t_Property ) )[ index ];
	}

	double CBSDFIntegrator::Abs( const Side t_Side, const double t_Theta, const double t_Phi ) {
		size_t index = m_Directions->getNearestBeamIndex( t_Theta, t_Phi );
		return ( *Abs( t_Side ) )[ index ];
	}

	double CBSDFIntegrator::Abs( const Side t_Side, const size_t Index ) {
		return ( *Abs( t_Side ) )[ Index ];
	}

	std::shared_ptr< const std::vector< double > > CBSDFIntegrator::lambdaVector() const {
		return m_Directions->lambdaVector();
	}

	std::shared_ptr< const CSquareMatrix > CBSDFIntegrator::lambdaMatrix() const {
		return m_Directions->lambdaMatrix();
	}

	double CBSDFIntegrator::integrate( CSquareMatrix const& t_Matrix ) const {
		double sum = 0;
		for ( size_t i = 0; i < m_DimMatrices; ++i ) {
			for ( size_t j = 0; j < m_DimMatrices; ++j ) {
				sum += t_Matrix[ i ][ j ] * ( *m_Directions )[ i ]->lambda() * ( *m_Directions )[ j ]->lambda();
			}
		}
		return sum / M_PI;
	}

	void CBSDFIntegrator::calcDiffuseDiffuse() {
		if ( !m_DiffuseDiffuseCalculated ) {
			for ( auto t_Side : EnumSide() ) {
				for ( auto t_Property : EnumPropertySimple() ) {
					m_MapDiffDiff( t_Side, t_Property ) = integrate( *getMatrix( t_Side, t_Property ) );
				}
			}
			m_DiffuseDiffuseCalculated = true;
		}
	}

	size_t CBSDFIntegrator::getNearestBeamIndex( const double t_Theta, const double t_Phi ) const {
		return m_Directions->getNearestBeamIndex( t_Theta, t_Phi );
	}

	void CBSDFIntegrator::calcHemispherical() {
		if ( !m_HemisphericalCalculated ) {
			for ( Side t_Side : EnumSide() ) {
				for ( PropertySimple t_Property : EnumPropertySimple() ) {
					m_Hem[ make_pair( t_Side, t_Property ) ] =
						m_Matrix.at( make_pair( t_Side, t_Property ) )->multVxM( *m_Directions->lambdaVector() );
				}
				m_Abs[ t_Side ] = make_shared< std::vector< double > >();
			}

			size_t size = m_Hem[ make_pair( Side::Front, PropertySimple::T ) ]->size();
			for ( size_t i = 0; i < size; ++i ) {
				for ( Side t_Side : EnumSide() ) {
					m_Abs.at( t_Side )->push_back( 1 - ( *m_Hem.at( make_pair( t_Side, PropertySimple::T ) ) )[ i ]
					                              - ( *m_Hem.at( make_pair( t_Side, PropertySimple::R ) ) )[ i ] );
				}
			}
			m_HemisphericalCalculated = true;
		}
	}

}
