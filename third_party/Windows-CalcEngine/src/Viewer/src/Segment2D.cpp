#include <cmath>
#include <algorithm>
#include <cassert>
#include <stdexcept>

#include "Segment2D.hpp"
#include "Point2D.hpp"
#include "WCECommon.hpp"
#include "ViewerConstants.hpp"


using namespace FenestrationCommon;

namespace Viewer {

	CSegment2D::CSegment2D( std::shared_ptr< const CPoint2D > const& t_StartPoint,
	                        std::shared_ptr< const CPoint2D > const& t_EndPoint ) :
		m_StartPoint( t_StartPoint ), m_EndPoint( t_EndPoint ) {
		calculateLength();
		calculateCenter();
	}

	std::shared_ptr< const CPoint2D > CSegment2D::startPoint() const {
		return m_StartPoint;
	}

	std::shared_ptr< const CPoint2D > CSegment2D::endPoint() const {
		return m_EndPoint;
	}

	std::shared_ptr< const CPoint2D > CSegment2D::centerPoint() const {
		return m_CenterPoint;
	}

	bool CSegment2D::operator==( CSegment2D const& rhs ) const {
		return m_StartPoint == rhs.m_StartPoint
			&& m_EndPoint == rhs.m_EndPoint;
	}

	bool CSegment2D::operator!=( CSegment2D const& rhs ) const {
		return !( *this == rhs );
	}

	double CSegment2D::length() const {
		return m_Length;
	}

	bool CSegment2D::intersectionWithSegment( std::shared_ptr< const CSegment2D > const& t_Segment ) const {
		auto aInt = false;
		if ( length() != 0 ) {
			auto aPoint = intersection( t_Segment );

			if ( aPoint != nullptr ) {
				aInt = isInRectangleRange( aPoint ) && t_Segment->isInRectangleRange( aPoint );
			}
		}

		return aInt;
	}

	IntersectionStatus CSegment2D::intersectionWithLine( std::shared_ptr< const CSegment2D > const& t_Segment ) const {
		auto status = IntersectionStatus::No;

		if ( length() != 0 ) {
			auto aPoint = intersection( t_Segment );

			if ( aPoint != nullptr ) {
				auto aInt = t_Segment->isInRectangleRange( aPoint );
				if ( aInt ) {
					status = IntersectionStatus::Segment;
				}
				if ( t_Segment->startPoint()->sameCoordinates( *aPoint ) ||
					t_Segment->endPoint()->sameCoordinates( *aPoint ) ) {
					status = IntersectionStatus::Point;
				}
			}
		}

		return status;
	}

	double CSegment2D::dotProduct( std::shared_ptr< const CSegment2D > const& t_Segment ) const {
		auto p1 = intensity();
		auto p2 = *t_Segment->intensity();

		return p1->dotProduct( p2 );
	}

	// Translates segment for given coordinates
	std::shared_ptr< CSegment2D > CSegment2D::translate( double const t_x, double const t_y ) const {
		auto startPoint = std::make_shared< CPoint2D >( m_StartPoint->x() + t_x, m_StartPoint->y() + t_y );
		auto endPoint = std::make_shared< CPoint2D >( m_EndPoint->x() + t_x, m_EndPoint->y() + t_y );
		auto aSegment = std::make_shared< CSegment2D >( startPoint, endPoint );
		return aSegment;
	}

	void CSegment2D::calculateLength() {
		auto deltaX = m_EndPoint->x() - m_StartPoint->x();
		auto deltaY = m_EndPoint->y() - m_StartPoint->y();
		m_Length = sqrt( deltaX * deltaX + deltaY * deltaY );
	}

	void CSegment2D::calculateCenter() {
		auto x = ( m_EndPoint->x() + m_StartPoint->x() ) / 2;
		auto y = ( m_EndPoint->y() + m_StartPoint->y() ) / 2;

		m_CenterPoint = std::make_shared< CPoint2D >( x, y );
	}

	std::shared_ptr< CPoint2D > CSegment2D::intersection( std::shared_ptr< const CSegment2D > const& t_Segment ) const {
		if ( t_Segment == nullptr ) {
			throw std::runtime_error( "Segment for intersection must be provided. Cannot operate with null segment." );
		}

		std::shared_ptr< CPoint2D > intersectionPoint = nullptr;

		auto A1 = coeffA();
		auto A2 = t_Segment->coeffA();
		auto B1 = coeffB();
		auto B2 = t_Segment->coeffB();

		auto C1 = coeffC();
		auto C2 = t_Segment->coeffC();
		auto x = 0.0;
		auto y = 0.0;

		if ( std::abs( A1 ) > ViewerConstants::DISTANCE_TOLERANCE ) {
			auto t1 = C2 - C1 * A2 / A1;
			auto t2 = B2 - B1 * A2 / A1;
			// assert( t2 != 0 );
			if ( std::abs( t2 ) > ViewerConstants::DISTANCE_TOLERANCE ) {
				y = t1 / t2;
				x = ( C1 - B1 * y ) / A1;
			}
			else {
				return intersectionPoint;
			}
		}
		else {
			y = C1 / B1;
			x = ( C2 - B2 * y ) / A2;
		}

		intersectionPoint = std::make_shared< CPoint2D >( x, y );

		return intersectionPoint;
	}

	bool CSegment2D::isInRectangleRange( std::shared_ptr< const CPoint2D > const& t_Point ) const {
		// Should return in range only if point is not exactly on the line's point
		auto inXRange = false;
		auto inYRange = false;

		// Check X range
		auto const maxX = std::max( m_EndPoint->x(), m_StartPoint->x() );
		auto const minX = std::min( m_EndPoint->x(), m_StartPoint->x() );
		if ( std::abs( maxX - minX ) > ViewerConstants::DISTANCE_TOLERANCE ) {
			if ( t_Point->x() < ( maxX - ViewerConstants::DISTANCE_TOLERANCE ) && t_Point->x() > ( minX + ViewerConstants::DISTANCE_TOLERANCE ) ) {
				inXRange = true;
			}
		}
		else {
			if ( std::abs( t_Point->x() - maxX ) < ViewerConstants::DISTANCE_TOLERANCE ) {
				inXRange = true;
			}
		}

		// Check Y range
		auto const maxY = std::max( m_EndPoint->y(), m_StartPoint->y() );
		auto const minY = std::min( m_EndPoint->y(), m_StartPoint->y() );
		if ( std::abs( maxY - minY ) > ViewerConstants::DISTANCE_TOLERANCE ) {
			if ( t_Point->y() < ( maxY - ViewerConstants::DISTANCE_TOLERANCE ) && t_Point->y() > ( minY + ViewerConstants::DISTANCE_TOLERANCE ) ) {
				inYRange = true;
			}
		}
		else {
			if ( std::abs( t_Point->y() - maxY ) < ViewerConstants::DISTANCE_TOLERANCE ) {
				inYRange = true;
			}
		}

		return inXRange && inYRange;
	}

	double CSegment2D::coeffA() const {
		return m_StartPoint->y() - m_EndPoint->y();
	}

	double CSegment2D::coeffB() const {
		return m_EndPoint->x() - m_StartPoint->x();
	}

	double CSegment2D::coeffC() const {
		return coeffB() * m_StartPoint->y() + coeffA() * m_StartPoint->x();
	}

	std::shared_ptr< CPoint2D > CSegment2D::intensity() const {
		auto x = m_EndPoint->x() - m_StartPoint->x();
		auto y = m_EndPoint->y() - m_StartPoint->y();
		auto aPoint = std::make_shared< CPoint2D >( x, y );
		return aPoint;
	}

}
