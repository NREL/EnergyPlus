#ifndef POINT2D_H
#define POINT2D_H

#include <memory>

namespace Viewer {

	////////////////////////////////////////////////////////////////////////////////////////
	// CPoint2D
	////////////////////////////////////////////////////////////////////////////////////////

	// Simple representation of point in Cartesian coordinate system
	class CPoint2D {
	public:
		CPoint2D( double const x, double const y );
		double x() const;
		double y() const;

		///////////////////////////////////////////////////////////////////////////////////
		// brief Test if two points are withing certain tolerance.
		// param t_Point 
		// return 
		///////////////////////////////////////////////////////////////////////////////////
		bool sameCoordinates( CPoint2D const& t_Point ) const;

		double dotProduct( CPoint2D const& t_Point ) const;

		bool operator==( CPoint2D const& rhs ) const;
		bool operator!=( CPoint2D const& rhs ) const;

		// True if current point is left from passed point (t_Point)
		bool isLeft( CPoint2D const& t_Point ) const;

		// Translates point for given coordinates
		std::shared_ptr< CPoint2D > translate( double const t_x, double const t_y ) const;

	protected:
		double m_x;
		double m_y;

	};

	////////////////////////////////////////////////////////////////////////////////////////
	// PointsProfile2DCompare
	////////////////////////////////////////////////////////////////////////////////////////

	// Compare position of the points relative to profile angle. Used in conjuction with stl algorithms for
	// point positioning.
	class PointsProfile2DCompare {
	public:
		explicit PointsProfile2DCompare( double const t_ProfileAngle );

		bool operator()( std::shared_ptr< const CPoint2D > const& t_Point1,
		                 std::shared_ptr< const CPoint2D > const& t_Point2 ) const;

	private:
		double m_ProfileAngle;
	};
}

#endif
