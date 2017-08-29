#ifndef VIEWSEGMENT2D_H
#define VIEWSEGMENT2D_H

#include <memory>
#include <vector>

#include "Segment2D.hpp"

namespace Viewer {

	enum class Shadowing { No = 0, Partial, Total };

	enum class PointPosition { Visible, Invisible, OnLine };

	class CViewSegment2D : public CSegment2D, public std::enable_shared_from_this< CViewSegment2D > {
	public:
		CViewSegment2D( std::shared_ptr< const CPoint2D > t_StartPoint,
		                std::shared_ptr< const CPoint2D > t_EndPoint );

		std::shared_ptr< const CViewSegment2D > getNormal();

		bool operator==( CViewSegment2D const& rhs ) const;
		bool operator!=( CViewSegment2D const& rhs ) const;

		// Calculates view factor coefficient. It needs to be divided by segment length to get real view factor.
		double viewFactorCoefficient( CSegment2D const& t_Segment ) const;

		// Self shadowing between two segments
		Shadowing selfShadowing( CViewSegment2D const& t_Segment ) const;

		// To determine position from the perspective of the segment
		PointPosition position( CPoint2D const& t_Point ) const;

		// Divide segment into number of subsegments
		std::shared_ptr< std::vector< std::shared_ptr< CViewSegment2D > > >
		subSegments( const size_t numSegments ) const;

		// Translates segment for given coordinates
		std::shared_ptr< CViewSegment2D > translate( const double t_x, const double t_y );

	private:
		// How much segment is self shadowed (No, Partial, Total)
		Shadowing isInSelfShadow( CViewSegment2D const& t_Segment ) const;

		void calculateNormal();

		// Normal to the segment
		std::shared_ptr< CViewSegment2D > m_Normal;
		bool m_NormalCalculated;

	};

}

#endif
