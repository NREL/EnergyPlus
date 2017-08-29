#include <cassert>
#include <stdexcept>

#include "ViewSegment2D.hpp"
#include "Point2D.hpp"
#include "ViewerConstants.hpp"



namespace Viewer {

	CViewSegment2D::CViewSegment2D( std::shared_ptr< const CPoint2D > t_StartPoint,
	                                std::shared_ptr< const CPoint2D > t_EndPoint ) : CSegment2D( t_StartPoint, t_EndPoint ),
	                                                                                 m_Normal( nullptr ), m_NormalCalculated( false ) {
	}

	std::shared_ptr< const CViewSegment2D > CViewSegment2D::getNormal() {
		if ( !m_NormalCalculated ) {
			calculateNormal();
			m_NormalCalculated = true;
		}
		return m_Normal;
	}

	bool CViewSegment2D::operator==( CViewSegment2D const& rhs ) const {
		return CSegment2D::operator==( rhs );
	}

	bool CViewSegment2D::operator!=( CViewSegment2D const& rhs ) const {
		return !( *this == rhs );
	}

	double CViewSegment2D::viewFactorCoefficient( CSegment2D const& t_Segment ) const {
		auto r11 = CSegment2D( m_StartPoint, t_Segment.endPoint() ).length();
		auto r22 = CSegment2D( m_EndPoint, t_Segment.startPoint() ).length();
		auto r12 = CSegment2D( m_StartPoint, t_Segment.startPoint() ).length();
		auto r21 = CSegment2D( m_EndPoint, t_Segment.endPoint() ).length();

		auto vFCoeff = r12 + r21 - r22 - r11;

		if ( vFCoeff < ViewerConstants::MIN_VIEW_COEFF ) {
			vFCoeff = 0;
		}

		return vFCoeff;
	}

	Shadowing CViewSegment2D::selfShadowing( CViewSegment2D const& t_Segment ) const {
		auto totalShadowing = Shadowing::Partial;
		auto vThis = isInSelfShadow( t_Segment );
		auto vOther = t_Segment.isInSelfShadow( *this );

		if ( vThis == Shadowing::Total || vOther == Shadowing::Total ) {
			totalShadowing = Shadowing::Total;
		}
		else if ( vThis == Shadowing::No && vOther == Shadowing::No ) {
			totalShadowing = Shadowing::No;
		}

		return totalShadowing;

	}

	Shadowing CViewSegment2D::isInSelfShadow( CViewSegment2D const& t_Segment ) const {
		auto numOfInvisibles = 0;

		auto visibilityStart = position( *t_Segment.startPoint() );
		auto visibilityEnd = position( *t_Segment.endPoint() );

		if ( visibilityStart == PointPosition::Invisible ) {
			++numOfInvisibles;
		}

		if ( visibilityEnd == PointPosition::Invisible ) {
			++numOfInvisibles;
		}

		if ( numOfInvisibles == 1 ) {
			if ( visibilityStart == PointPosition::OnLine || visibilityEnd == PointPosition::OnLine ) {
				++numOfInvisibles;
			}
		}

		return Shadowing( numOfInvisibles );
	}

	std::shared_ptr< std::vector< std::shared_ptr< CViewSegment2D > > > CViewSegment2D::subSegments( const size_t numSegments ) const {
		if ( numSegments == 0 ) {
			throw std::runtime_error( "Number of subsegments must be greater than zero." );
		}
		std::shared_ptr< std::vector< std::shared_ptr< CViewSegment2D > > > subSegments =
			std::make_shared< std::vector< std::shared_ptr< CViewSegment2D > > >();
		double dX = ( m_EndPoint->x() - m_StartPoint->x() ) / numSegments;
		double dY = ( m_EndPoint->y() - m_StartPoint->y() ) / numSegments;
		double startX = m_StartPoint->x();
		double startY = m_StartPoint->y();
		std::shared_ptr< CPoint2D > sPoint = std::make_shared< CPoint2D >( startX, startY );
		for ( size_t i = 1; i <= numSegments; ++i ) {
			std::shared_ptr< CPoint2D > ePoint = std::make_shared< CPoint2D >( startX + i * dX, startY + i * dY );
			std::shared_ptr< CViewSegment2D > aSegment = std::make_shared< CViewSegment2D >( sPoint, ePoint );
			subSegments->push_back( aSegment );
			sPoint = ePoint;
		}

		return subSegments;
	}

	std::shared_ptr< CViewSegment2D > CViewSegment2D::translate( const double t_x, const double t_y ) {
		std::shared_ptr< CSegment2D > aSegment = CSegment2D::translate( t_x, t_y );
		return std::make_shared< CViewSegment2D >( aSegment->startPoint(), aSegment->endPoint() );
	}

	PointPosition CViewSegment2D::position( CPoint2D const& t_Point ) const {
		auto aPosition = PointPosition::OnLine;

		if ( !( t_Point.sameCoordinates( *m_StartPoint ) || t_Point.sameCoordinates( *m_EndPoint ) ) ) {
			auto dx = m_EndPoint->x() - m_StartPoint->x();
			auto dy = m_EndPoint->y() - m_StartPoint->y();
			auto position = dx * ( t_Point.y() - m_StartPoint->y() ) - dy * ( t_Point.x() - m_StartPoint->x() );
			if ( position > ViewerConstants::DISTANCE_TOLERANCE ) {
				aPosition = PointPosition::Invisible;
			}
			else if ( position < -ViewerConstants::DISTANCE_TOLERANCE ) {
				aPosition = PointPosition::Visible;
			}
		}

		return aPosition;
	}

	void CViewSegment2D::calculateNormal() {
		assert( length() > 0 );
		double xn = ( m_EndPoint->y() - m_StartPoint->y() ) / length();
		double yn = ( m_StartPoint->x() - m_EndPoint->x() ) / length();
		std::shared_ptr< CPoint2D > startPoint = std::make_shared< CPoint2D >( 0, 0 ); // normal always starts from (0, 0)
		std::shared_ptr< CPoint2D > endPoint = std::make_shared< CPoint2D >( xn, yn );
		m_Normal = std::make_shared< CViewSegment2D >( startPoint, endPoint );
	}

}
