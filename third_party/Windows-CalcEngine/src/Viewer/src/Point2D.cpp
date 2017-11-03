#include "Point2D.hpp"
#include "ViewerConstants.hpp"
#include "WCECommon.hpp"


using namespace FenestrationCommon;

namespace Viewer {

	////////////////////////////////////////////////////////////////////////////////////////
	// CPoint2D
	////////////////////////////////////////////////////////////////////////////////////////

	CPoint2D::CPoint2D( double const x, double const y ) : m_x( x ), m_y( y ) {

	}

	double CPoint2D::x() const {
		return m_x;
	}

	double CPoint2D::y() const {
		return m_y;
	}

	bool CPoint2D::sameCoordinates( CPoint2D const& t_Point ) const {
		return ( std::abs( t_Point.x() - m_x ) < ViewerConstants::DISTANCE_TOLERANCE ) &&
			( std::abs( t_Point.y() - m_y ) < ViewerConstants::DISTANCE_TOLERANCE );
	}

	double CPoint2D::dotProduct( CPoint2D const& t_Point ) const {
		return m_x * t_Point.x() + m_y * t_Point.y();
	}

	bool CPoint2D::operator==( CPoint2D const& rhs ) const {
		return m_x == rhs.m_x
			&& m_y == rhs.m_y;
	}

	bool CPoint2D::operator!=( CPoint2D const& rhs ) const {
		return !( *this == rhs );
	}

	bool CPoint2D::isLeft( CPoint2D const& t_Point ) const {
		return m_x < t_Point.x();
	}

	std::shared_ptr< CPoint2D > CPoint2D::translate( double const t_x, double const t_y ) const {
		auto aPoint = std::make_shared< CPoint2D >( m_x + t_x, m_y + t_y );
		return aPoint;
	}

	////////////////////////////////////////////////////////////////////////////////////////
	// PointsCompare
	////////////////////////////////////////////////////////////////////////////////////////

	PointsProfile2DCompare::PointsProfile2DCompare( double const t_ProfileAngle ) :
		m_ProfileAngle( t_ProfileAngle ) {

	}

	bool PointsProfile2DCompare::operator()( std::shared_ptr< const CPoint2D > const& t_Point1,
	                                         std::shared_ptr< const CPoint2D > const& t_Point2 ) const {
		auto isHigher = false;
		if ( m_ProfileAngle != 0 ) {
			const auto tanPhi = tan( radians( m_ProfileAngle ) );
			if ( tanPhi > 0 ) {
				isHigher = ( t_Point1->x() - t_Point1->y() / tanPhi ) < ( t_Point2->x() - t_Point2->y() / tanPhi );
			}
			else {
				isHigher = ( t_Point1->x() - t_Point1->y() / tanPhi ) > ( t_Point2->x() - t_Point2->y() / tanPhi );
			}
		}
		else {
			isHigher = t_Point1->y() > t_Point2->y();
		}
		return isHigher;

	}

}
