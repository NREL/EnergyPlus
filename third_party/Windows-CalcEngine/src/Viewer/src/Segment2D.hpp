#ifndef SEGMENT2D_H
#define SEGMENT2D_H

#include <memory>

namespace Viewer {

	class CPoint2D;

	// Intersection status is used to determine how line intersects the segment.
	// No - line does not intersect the segment
	// Point - line intersects the segment excatly at one of the points
	// Segment - line intersect the segment somewhere in the middle
	enum class IntersectionStatus { No, Point, Segment };

	// Representation of simple Segment in 2D space
	class CSegment2D {
	public:
		CSegment2D( std::shared_ptr< const CPoint2D > const& t_StartPoint,
		            std::shared_ptr< const CPoint2D > const& t_EndPoint );

		std::shared_ptr< const CPoint2D > startPoint() const;
		std::shared_ptr< const CPoint2D > endPoint() const;
		std::shared_ptr< const CPoint2D > centerPoint() const;

		bool operator==( CSegment2D const& rhs ) const;
		bool operator!=( CSegment2D const& rhs ) const;

		double length() const;

		// Calculates if segment intersects with passed t_Segment
		bool intersectionWithSegment( std::shared_ptr< const CSegment2D > const& t_Segment ) const;

		// For line made up of current segment, calculate how it intersects passed segment.
		IntersectionStatus intersectionWithLine( std::shared_ptr< const CSegment2D > const& t_Segment ) const;

		// Dot product of two std::vectors
		double dotProduct( std::shared_ptr< const CSegment2D > const& t_Segment ) const;

		// Translates segment for given coordinates
		std::shared_ptr< CSegment2D > translate( double const t_x, double const t_y ) const;

		// returns end point of the std::vector that starts at (0, 0)
		std::shared_ptr< CPoint2D > intensity() const;

	protected:
		std::shared_ptr< const CPoint2D > m_StartPoint;
		std::shared_ptr< const CPoint2D > m_EndPoint;
		std::shared_ptr< const CPoint2D > m_CenterPoint;

	private:
		void calculateLength();
		void calculateCenter();

		// Calculates intesection point between two lines made up of std::vector. Even if two std::vectors do not
		// intersect, point of intersection will be returned
		std::shared_ptr< CPoint2D > intersection( std::shared_ptr< const CSegment2D > const& t_Segment ) const;

		// test if x coordinate is in range of rectangle made up of segment
		bool isInRectangleRange( std::shared_ptr< const CPoint2D > const& t_Point ) const;

		// Equation of LINE made up of two points
		// A, B and C coefficients of line that is made up of segment. Line equation is A*x + B*y = C
		double coeffA() const;
		double coeffB() const;
		double coeffC() const;

		double m_Length;
	};

}

#endif
