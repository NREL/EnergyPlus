#include <cassert>
#include <algorithm>

#include "Geometry2DBeam.hpp"
#include "Geometry2D.hpp"
#include "ViewSegment2D.hpp"
#include "Segment2D.hpp"
#include "Point2D.hpp"
#include "ViewerConstants.hpp"
#include "WCECommon.hpp"


using namespace FenestrationCommon;

namespace Viewer {

	////////////////////////////////////////////////////////////////////////////////////////
	// BeamViewFactor
	////////////////////////////////////////////////////////////////////////////////////////
	BeamViewFactor::BeamViewFactor( size_t const t_Geometry2DIndex, size_t const t_SegmentIndex,
	                                double const t_Value, double const t_PercentHit ) : enclosureIndex( t_Geometry2DIndex ),
	                                                                                    segmentIndex( t_SegmentIndex ), value( t_Value ), percentHit( t_PercentHit ) {

	}

	bool BeamViewFactor::operator==( BeamViewFactor const& t_BVF ) const {
		return ( t_BVF.enclosureIndex == this->enclosureIndex ) &&
			( t_BVF.segmentIndex == this->segmentIndex );
	}


	////////////////////////////////////////////////////////////////////////////////////////
	// CDirect2DBeam
	////////////////////////////////////////////////////////////////////////////////////////
	CDirect2DBeam::CDirect2DBeam( std::shared_ptr< const CViewSegment2D > const& t_Beam ) : m_Beam( t_Beam ) {
		if ( t_Beam == nullptr ) {
			throw std::runtime_error( "Direct beam must have correct beam assigned." );
		}
		m_Segments = std::make_shared< std::vector< std::shared_ptr< const CViewSegment2D > > >();
	}

	// Check if segment intersects with the beam
	void CDirect2DBeam::checkSegment( std::shared_ptr< const CViewSegment2D > const& t_Segment ) const {
		auto aStatus = m_Beam->intersectionWithLine( t_Segment );
		if ( aStatus != IntersectionStatus::No ) {
			m_Segments->push_back( t_Segment );
		}
	}

	double CDirect2DBeam::Side() const {
		assert( m_Beam != nullptr );
		return m_Beam->startPoint()->y();
	}

	std::shared_ptr< const CViewSegment2D > CDirect2DBeam::getClosestCommonSegment(
		std::shared_ptr< const CDirect2DBeam > const& t_Beam ) const {
		std::shared_ptr< const CViewSegment2D > aSegment = nullptr;
		for ( auto thisSegment : *m_Segments ) {
			if ( t_Beam->isSegmentIn( thisSegment ) ) {
				if ( aSegment == nullptr ) {
					aSegment = thisSegment;
				}
				else {
					if ( aSegment->centerPoint()->x() > thisSegment->centerPoint()->x() ) {
						aSegment = thisSegment;
					}
				}
			}
		}

		return aSegment;
	}

	double CDirect2DBeam::cosAngle( std::shared_ptr< const CViewSegment2D > const& t_Segment ) const {
		assert( m_Beam != nullptr );
		return m_Beam->dotProduct( t_Segment ) / m_Beam->length();
	}

	bool CDirect2DBeam::isSegmentIn( std::shared_ptr< const CViewSegment2D > const& t_Segment ) const {
		auto isIn = false;
		for ( auto thisSegment : *m_Segments ) {
			if ( thisSegment == t_Segment ) {
				isIn = true;
				break;
			}
		}
		return isIn;
	}

	////////////////////////////////////////////////////////////////////////////////////////
	// CDirect2DRay
	////////////////////////////////////////////////////////////////////////////////////////

	CDirect2DRay::CDirect2DRay( std::shared_ptr< CDirect2DBeam > const& t_Beam1,
	                            std::shared_ptr< CDirect2DBeam > const& t_Beam2 ) : m_Beam1( t_Beam1 ), m_Beam2( t_Beam2 ) {
		if ( t_Beam1 == nullptr ) {
			throw std::runtime_error( "Beam number one of the ray is not correctly created." );
		}
		if ( t_Beam2 == nullptr ) {
			throw std::runtime_error( "Beam number two of the ray is not correctly created." );
		}
	}

	CDirect2DRay::CDirect2DRay( std::shared_ptr< CViewSegment2D > const& t_Ray1,
	                            std::shared_ptr< CViewSegment2D > const& t_Ray2 ) {
		if ( t_Ray1 == nullptr ) {
			throw std::runtime_error( "Ray number one of the ray is not correctly created." );
		}
		if ( t_Ray2 == nullptr ) {
			throw std::runtime_error( "Ray number two of the ray is not correctly created." );
		}
		m_Beam1 = std::make_shared< CDirect2DBeam >( t_Ray1 );
		m_Beam2 = std::make_shared< CDirect2DBeam >( t_Ray2 );
	}

	double CDirect2DRay::rayNormalHeight() const {
		assert( m_Beam1 != nullptr );
		assert( m_Beam2 != nullptr );
		return m_Beam1->Side() - m_Beam2->Side();
	}

	void CDirect2DRay::checkSegment( std::shared_ptr< const CViewSegment2D > const& t_Segment ) const {
		assert( m_Beam1 != nullptr );
		assert( m_Beam2 != nullptr );
		m_Beam1->checkSegment( t_Segment );
		m_Beam2->checkSegment( t_Segment );
	}

	// Return segment hit by the ray
	std::shared_ptr< const CViewSegment2D > CDirect2DRay::closestSegmentHit() const {
		return m_Beam1->getClosestCommonSegment( m_Beam2 );
	}

	double CDirect2DRay::cosAngle( std::shared_ptr< const CViewSegment2D > const& t_Segment ) const {
		assert( m_Beam1 != nullptr );
		return m_Beam1->cosAngle( t_Segment );
	}

	////////////////////////////////////////////////////////////////////////////////////////
	// CDirect2DRayResult
	////////////////////////////////////////////////////////////////////////////////////////

	CDirect2DRaysResult::CDirect2DRaysResult( double const t_ProfileAngle, double const t_DirectToDirect,
	                                          std::shared_ptr< std::vector< BeamViewFactor > > const& t_BeamViewFactors ) : m_ViewFactors( t_BeamViewFactors ),
	                                                                                                              m_DirectToDirect( t_DirectToDirect ), m_ProfileAngle( t_ProfileAngle ) {

	}

	std::shared_ptr< std::vector< BeamViewFactor > > CDirect2DRaysResult::beamViewFactors() const {
		return m_ViewFactors;
	}

	double CDirect2DRaysResult::directToDirect() const {
		return m_DirectToDirect;
	}

	double CDirect2DRaysResult::profileAngle() const {
		return m_ProfileAngle;
	}

	////////////////////////////////////////////////////////////////////////////////////////
	// CDirect2DRayResults
	////////////////////////////////////////////////////////////////////////////////////////

	CDirect2DRaysResults::CDirect2DRaysResults() :
		m_Results( std::make_shared< std::vector< std::shared_ptr< CDirect2DRaysResult > > >() ) {

	}

	std::shared_ptr< CDirect2DRaysResult > CDirect2DRaysResults::getResult( double const t_ProfileAngle ) {
		std::shared_ptr< CDirect2DRaysResult > Result = nullptr;

		auto it = find_if( m_Results->begin(), m_Results->end(),
		                   [ &t_ProfileAngle ]( std::shared_ptr< CDirect2DRaysResult > const& obj ) {
		                   return fabs( obj->profileAngle() - t_ProfileAngle ) < 1e-6;
	                   } );

		if ( it != m_Results->end() ) {
			Result = *it;
		}

		return Result;
	}

	std::shared_ptr< CDirect2DRaysResult > CDirect2DRaysResults::append( double const t_ProfileAngle,
	                                                                     double const t_DirectToDirect, std::shared_ptr< std::vector< BeamViewFactor > > const& t_BeamViewFactor ) const {
		auto aResult = std::make_shared< CDirect2DRaysResult >( t_ProfileAngle, t_DirectToDirect, t_BeamViewFactor );
		m_Results->push_back( aResult );
		return aResult;
	}

	void CDirect2DRaysResults::clear() const {
		m_Results->clear();
	}

	////////////////////////////////////////////////////////////////////////////////////////
	// CDirect2DRays
	////////////////////////////////////////////////////////////////////////////////////////

	CDirect2DRays::CDirect2DRays( Side const t_Side ) : m_Side( t_Side ) {
		m_LowerRay = nullptr;
		m_UpperRay = nullptr;
		m_CurrentResult = nullptr;
	}

	void CDirect2DRays::appendGeometry2D( std::shared_ptr< const CGeometry2D > const& t_Geometry2D ) {
		m_Geometries2D.push_back( t_Geometry2D );
		m_Results.clear();
	}

	std::shared_ptr< std::vector< BeamViewFactor > > CDirect2DRays::beamViewFactors( double const t_ProfileAngle ) {
		calculateAllProperties( t_ProfileAngle );
		assert( m_CurrentResult != nullptr );
		return m_CurrentResult->beamViewFactors();
	}

	double CDirect2DRays::directToDirect( double const t_ProfileAngle ) {
		calculateAllProperties( t_ProfileAngle );
		assert( m_CurrentResult != nullptr );
		return m_CurrentResult->directToDirect();
	}

	void CDirect2DRays::calculateAllProperties( double const t_ProfileAngle ) {
		if ( m_CurrentResult != nullptr && m_CurrentResult->profileAngle() != t_ProfileAngle ) {
			m_CurrentResult = m_Results.getResult( t_ProfileAngle );
		}
		if ( m_CurrentResult == nullptr ) {
			findRayBoundaries( t_ProfileAngle );
			findInBetweenRays( t_ProfileAngle );
			calculateBeamProperties( t_ProfileAngle );
		}
	}

	void CDirect2DRays::findRayBoundaries( double const t_ProfileAngle ) {
		std::shared_ptr< CViewSegment2D > entryRay = nullptr;
		for ( auto aGeometry : m_Geometries2D ) {
			// TODO: Geometry depends on entry or exit points
			std::shared_ptr< const CPoint2D > aPoint = nullptr;
			switch ( m_Side ) {
			case Side::Front:
				aPoint = aGeometry->entryPoint();
				break;
			case Side::Back:
				aPoint = aGeometry->exitPoint();
				break;
			default:
				assert("Incorrect assignement of ray position.");
				break;
			}
			entryRay = createSubBeam( *aPoint, t_ProfileAngle );
			if ( aGeometry == *m_Geometries2D.begin() ) {
				m_LowerRay = entryRay;
				m_UpperRay = entryRay;
			}
			else {
				// This sets profile angle for point comparison that follows in next lines
				auto aProfilePoint = PointsProfile2DCompare( t_ProfileAngle );
				if ( aProfilePoint( m_LowerRay->startPoint(), entryRay->startPoint() ) ) {
					m_LowerRay = entryRay;
				}
				if ( !aProfilePoint( m_UpperRay->startPoint(), entryRay->startPoint() ) ) {
					m_UpperRay = entryRay;
				}
			}
		}
	}

	void CDirect2DRays::findInBetweenRays( double const t_ProfileAngle ) {

		std::vector< std::shared_ptr< const CPoint2D > > inBetweenPoints;

		// m_Beams.push_back( m_UpperRay );
		for ( auto aEnclosure : m_Geometries2D ) {
			auto aSegments = aEnclosure->segments();
			if ( isInRay( *( *aSegments )[ 0 ]->startPoint() ) ) {
				inBetweenPoints.push_back( ( *aSegments )[ 0 ]->startPoint() );
			}
			for ( auto aSegment : *aSegments ) {
				auto endPoint = aSegment->endPoint();
				// Ray is alway going from left to right. For point to be in between beam, it must be visible 
				// for upper ray and invisible for lower ray
				if ( m_UpperRay->position( *endPoint ) == PointPosition::Visible &&
					m_LowerRay->position( *endPoint ) == PointPosition::Invisible ) {
					inBetweenPoints.push_back( endPoint );
				}
			}
		}

		m_Rays.clear();

		sort( inBetweenPoints.begin(), inBetweenPoints.end(), PointsProfile2DCompare( t_ProfileAngle ) );

		// Creating incoming rays
		auto firstBeam = m_UpperRay;
		std::shared_ptr< CViewSegment2D > secondBeam = nullptr;
		for ( auto aPoint : inBetweenPoints ) {
			secondBeam = createSubBeam( *aPoint, t_ProfileAngle );
			auto aRay = std::make_shared< CDirect2DRay >( firstBeam, secondBeam );

			// Dont save rays that are smaller than distance tolerance
			if ( aRay->rayNormalHeight() > ViewerConstants::DISTANCE_TOLERANCE ) {
				m_Rays.push_back( aRay );
			}
			firstBeam = secondBeam;
		}
		auto aRay = std::make_shared< CDirect2DRay >( firstBeam, m_LowerRay );
		m_Rays.push_back( aRay );
	}

	void CDirect2DRays::calculateBeamProperties( double const t_ProfileAngle ) {
		// First check all segments and calculte total ray height
		auto totalHeight = 0.0;
		for ( auto beamRay : m_Rays ) {
			totalHeight += beamRay->rayNormalHeight();
			for ( auto aEnclosure : m_Geometries2D ) {
				for ( auto aSegment : ( *aEnclosure->segments() ) ) {
					beamRay->checkSegment( aSegment );
				}
			}
		}

		// Now calculate beam view factors
		auto aViewFactors = std::make_shared< std::vector< BeamViewFactor > >();
		double aDirectToDirect = 0;
		// Create beam direction parallel to x-axe
		auto sPoint = std::make_shared< CPoint2D >( 0, 0 );
		auto ePoint = std::make_shared< CPoint2D >( 1, 0 );
		auto aNormalBeamDirection = std::make_shared< CViewSegment2D >( sPoint, ePoint );
		for ( auto beamRay : m_Rays ) {
			auto currentHeight = beamRay->rayNormalHeight();
			auto projectedBeamHeight = beamRay->cosAngle( aNormalBeamDirection );
			auto viewFactor = 0.0;
			auto percentHit = 0.0;
			auto closestSegment = beamRay->closestSegmentHit();
			for ( size_t e = 0; e < m_Geometries2D.size(); ++e ) {
				for ( size_t s = 0; s < m_Geometries2D[ e ]->segments()->size(); ++s ) {
					auto currentSegment = ( *m_Geometries2D[ e ]->segments() )[ s ];
					if ( currentSegment == beamRay->closestSegmentHit() ) {
						viewFactor = currentHeight / totalHeight;
						projectedBeamHeight = projectedBeamHeight * currentHeight;
						auto segmentHitLength = projectedBeamHeight / fabs( beamRay->cosAngle( currentSegment->getNormal() ) );
						percentHit = segmentHitLength / currentSegment->length();
						auto aTest = find( aViewFactors->begin(),
						                   aViewFactors->end(), BeamViewFactor( e, s, 0, 0 ) );
						if ( aTest != aViewFactors->end() ) {
							auto& aVF = *aTest;
							aVF.value += viewFactor;
							aVF.percentHit += percentHit;
						}
						else {
							auto aVF = BeamViewFactor( e, s, viewFactor, percentHit );
							aViewFactors->push_back( aVF );
						}
					}
				}
			}

			// No segment is being hit. That means ray goes through. Add this to total view factor
			if ( viewFactor == 0 ) {
				aDirectToDirect += currentHeight / totalHeight;
			}
		}
		m_CurrentResult = m_Results.append( t_ProfileAngle, aDirectToDirect, aViewFactors );
	}

	bool CDirect2DRays::isInRay( CPoint2D const& t_Point ) const {
		assert( m_UpperRay != nullptr );
		assert( m_LowerRay != nullptr );
		return m_UpperRay->position( t_Point ) == PointPosition::Visible &&
			m_LowerRay->position( t_Point ) == PointPosition::Invisible;
	}

	std::shared_ptr< CViewSegment2D > CDirect2DRays::createSubBeam( CPoint2D const& t_Point,
	                                                           double const t_ProfileAngle ) const {
		std::shared_ptr< CViewSegment2D > subSegment = nullptr;
		auto const deltaX = 10.0;
		auto const tanPhi = tan( radians( t_ProfileAngle ) );
		auto yStart = t_Point.y() - t_Point.x() * tanPhi;
		auto yEnd = yStart + deltaX * tanPhi;

		auto startPoint = std::make_shared< CPoint2D >( 0, yStart );
		auto endPoint = std::make_shared< CPoint2D >( deltaX, yEnd );
		return std::make_shared< CViewSegment2D >( startPoint, endPoint );
	}

	////////////////////////////////////////////////////////////////////////////////////////
	// CGeometry2DBeam
	////////////////////////////////////////////////////////////////////////////////////////

	CGeometry2DBeam::CGeometry2DBeam() : m_Incoming( Side::Front ), m_Outgoing( Side::Back ) {

	}

	void CGeometry2DBeam::appendGeometry2D( std::shared_ptr< const CGeometry2D > const& t_Geometry2D ) {
		m_Incoming.appendGeometry2D( t_Geometry2D );
		m_Outgoing.appendGeometry2D( t_Geometry2D );
	}

	// Returns non zero view factors. It also calculates direct to direct component for the beam
	std::shared_ptr< std::vector< BeamViewFactor > > CGeometry2DBeam::beamViewFactors( double const t_ProfileAngle,
	                                                                         Side const t_Side ) {
		auto aRay = getRay( t_Side );
		return aRay->beamViewFactors( t_ProfileAngle );
	}

	double CGeometry2DBeam::directToDirect( double const t_ProfileAngle, Side const t_Side ) {
		auto aRay = getRay( t_Side );
		return aRay->directToDirect( t_ProfileAngle );
	}

	CDirect2DRays* CGeometry2DBeam::getRay( Side const t_Side ) {
		CDirect2DRays* aRay = nullptr;
		switch ( t_Side ) {
		case Side::Front:
			return aRay = &m_Incoming;
			break;
		case Side::Back:
			return aRay = &m_Outgoing;
			break;
		default:
			assert("Incorrect assignement of ray position.");
			break;
		}
		return aRay;
	}

}
