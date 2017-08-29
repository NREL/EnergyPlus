#include <cassert>
#include <algorithm>

#include "BSDFDirections.hpp"
#include "BSDFPatch.hpp"
#include "BSDFThetaLimits.hpp"
#include "BSDFPhiLimits.hpp"
#include "WCECommon.hpp"

using namespace std;
using namespace FenestrationCommon;

namespace SingleLayerOptics {

	/////////////////////////////////////////////////////////////////
	///  CBSDFDefinition
	/////////////////////////////////////////////////////////////////

	CBSDFDefinition::CBSDFDefinition( const double t_Theta, const size_t t_NumOfPhis ) :
		m_Theta( t_Theta ), m_NumOfPhis( t_NumOfPhis ) {
	}

	double CBSDFDefinition::theta() const {
		return m_Theta;
	}

	size_t CBSDFDefinition::numOfPhis() const {
		return m_NumOfPhis;
	}

	/////////////////////////////////////////////////////////////////
	///  CBSDFDirections
	/////////////////////////////////////////////////////////////////

	CBSDFDirections::CBSDFDirections( std::vector< CBSDFDefinition >& t_Definitions, const BSDFHemisphere t_Side ) {
		vector< double > thetaAngles;
		vector< size_t > numPhiAngles;
		for ( auto it = t_Definitions.begin(); it < t_Definitions.end(); ++it ) {
			thetaAngles.push_back( ( *it ).theta() );
			numPhiAngles.push_back( ( *it ).numOfPhis() );
		}

		CThetaLimits ThetaLimits( thetaAngles );
		vector< double > thetaLimits = *ThetaLimits.getThetaLimits();

		double lowerTheta = thetaLimits[ 0 ];
		for ( size_t i = 1; i < thetaLimits.size(); ++i ) {
			double upperTheta = thetaLimits[ i ];
			std::shared_ptr< CAngleLimits > currentTheta = nullptr;
			if ( i == 1 ) {
				currentTheta = make_shared< CCentralAngleLimits >( upperTheta );
			}
			else {
				currentTheta = make_shared< CAngleLimits >( lowerTheta, upperTheta );
			}


			CPhiLimits phiAngles( numPhiAngles[ i - 1 ] );
			vector< double > phiLimits = *phiAngles.getPhiLimits();
			double lowerPhi = phiLimits[ 0 ];
			if ( t_Side == BSDFHemisphere::Outgoing ) {
				lowerPhi += 180;
			}
			for ( size_t j = 1; j < phiLimits.size(); ++j ) {
				double upperPhi = phiLimits[ j ];
				if ( t_Side == BSDFHemisphere::Outgoing ) {
					upperPhi += 180;
				}
				std::shared_ptr< CAngleLimits > currentPhi = make_shared< CAngleLimits >( lowerPhi, upperPhi );
				std::shared_ptr< CBSDFPatch > currentPatch = make_shared< CBSDFPatch >( currentTheta, currentPhi );
				m_Patches.push_back( currentPatch );
				lowerPhi = upperPhi;
			}
			lowerTheta = upperTheta;
		}

		// build lambda vector and matrix
		size_t size = m_Patches.size();
		m_LambdaVector = make_shared< std::vector< double > >();
		m_LambdaMatrix = make_shared< CSquareMatrix >( size );
		for ( size_t i = 0; i < size; ++i ) {
			m_LambdaVector->push_back( m_Patches[ i ]->lambda() );
			( *m_LambdaMatrix )[ i ][ i ] = m_Patches[ i ]->lambda();
		}

	}

	size_t CBSDFDirections::size() const {
		return m_Patches.size();
	}

	std::shared_ptr< const CBSDFPatch > CBSDFDirections::operator[]( const size_t Index ) const {
		return m_Patches[ Index ];
	}

	vector< std::shared_ptr< CBSDFPatch > >::iterator CBSDFDirections::begin() {
		return m_Patches.begin();
	}

	vector< std::shared_ptr< CBSDFPatch > >::iterator CBSDFDirections::end() {
		return m_Patches.end();
	}

	std::shared_ptr< std::vector< double > > CBSDFDirections::lambdaVector() const {
		return m_LambdaVector;
	}

	std::shared_ptr< CSquareMatrix > CBSDFDirections::lambdaMatrix() const {
		return m_LambdaMatrix;
	}

	size_t CBSDFDirections::getNearestBeamIndex( const double t_Theta, const double t_Phi ) const {
		auto it = std::find_if( m_Patches.begin(), m_Patches.end(),
		                        [ & ]( const std::shared_ptr< CBSDFPatch >& a ) {
		                        return a->isInPatch( t_Theta, t_Phi );
	                        } );

		size_t index = std::distance( m_Patches.begin(), it );
		return index;
	}

	/////////////////////////////////////////////////////////////////
	///  CBSDFHemisphere
	/////////////////////////////////////////////////////////////////

	CBSDFHemisphere::CBSDFHemisphere( const BSDFBasis t_Basis ) {
		vector< CBSDFDefinition > aDefinitions;
		switch ( t_Basis ) {
		case BSDFBasis::Small:
			aDefinitions.push_back( CBSDFDefinition( 0, 1 ) );
			aDefinitions.push_back( CBSDFDefinition( 13, 1 ) );
			aDefinitions.push_back( CBSDFDefinition( 26, 1 ) );
			aDefinitions.push_back( CBSDFDefinition( 39, 1 ) );
			aDefinitions.push_back( CBSDFDefinition( 52, 1 ) );
			aDefinitions.push_back( CBSDFDefinition( 65, 1 ) );
			aDefinitions.push_back( CBSDFDefinition( 80.75, 1 ) );
			break;
		case BSDFBasis::Quarter:
			aDefinitions.push_back( CBSDFDefinition( 0, 1 ) );
			aDefinitions.push_back( CBSDFDefinition( 18, 8 ) );
			aDefinitions.push_back( CBSDFDefinition( 36, 12 ) );
			aDefinitions.push_back( CBSDFDefinition( 54, 12 ) );
			aDefinitions.push_back( CBSDFDefinition( 76.5, 8 ) );
			break;
		case BSDFBasis::Half:
			aDefinitions.push_back( CBSDFDefinition( 0, 1 ) );
			aDefinitions.push_back( CBSDFDefinition( 13, 8 ) );
			aDefinitions.push_back( CBSDFDefinition( 26, 12 ) );
			aDefinitions.push_back( CBSDFDefinition( 39, 16 ) );
			aDefinitions.push_back( CBSDFDefinition( 52, 20 ) );
			aDefinitions.push_back( CBSDFDefinition( 65, 12 ) );
			aDefinitions.push_back( CBSDFDefinition( 80.75, 8 ) );
			break;
		case BSDFBasis::Full:
			aDefinitions.push_back( CBSDFDefinition( 0, 1 ) );
			aDefinitions.push_back( CBSDFDefinition( 10, 8 ) );
			aDefinitions.push_back( CBSDFDefinition( 20, 16 ) );
			aDefinitions.push_back( CBSDFDefinition( 30, 20 ) );
			aDefinitions.push_back( CBSDFDefinition( 40, 24 ) );
			aDefinitions.push_back( CBSDFDefinition( 50, 24 ) );
			aDefinitions.push_back( CBSDFDefinition( 60, 24 ) );
			aDefinitions.push_back( CBSDFDefinition( 70, 16 ) );
			aDefinitions.push_back( CBSDFDefinition( 82.5, 12 ) );
			break;
		default:
			throw runtime_error( "Incorrect definiton of the basis." );
			break;
		}
		m_IncomingDirections = make_shared< CBSDFDirections >( aDefinitions, BSDFHemisphere::Incoming );
		m_OutgoingDirections = make_shared< CBSDFDirections >( aDefinitions, BSDFHemisphere::Outgoing );
	}

	CBSDFHemisphere::CBSDFHemisphere( std::vector< CBSDFDefinition >& t_Definitions ) :
		m_IncomingDirections( make_shared< CBSDFDirections >( t_Definitions, BSDFHemisphere::Incoming ) ),
		m_OutgoingDirections( make_shared< CBSDFDirections >( t_Definitions, BSDFHemisphere::Outgoing ) ) {
	}

	std::shared_ptr< const CBSDFDirections > CBSDFHemisphere::getDirections( const BSDFHemisphere t_Side ) const {
		std::shared_ptr< CBSDFDirections > aDirections = nullptr;
		switch ( t_Side ) {
		case BSDFHemisphere::Incoming:
			aDirections = m_IncomingDirections;
			break;
		case BSDFHemisphere::Outgoing:
			aDirections = m_OutgoingDirections;
			break;
		default:
			assert("Incorrect selection of BSDF Hemisphere.");
			break;
		}
		return aDirections;
	}

}
