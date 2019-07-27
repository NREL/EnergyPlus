#ifndef VENETIANSLAT_H
#define VENETIANSLAT_H

#include <memory>

namespace Viewer {

	class CGeometry2D;
	class CSegment2D;

}

namespace SingleLayerOptics {

	enum class SegmentsDirection { Positive, Negative };

	class CVenetianSlat {
	public:
		CVenetianSlat( const double t_SlatWidth, const double t_SlatSpacing, const double t_SlatTiltAngle,
		               const double t_CurvatureRadius, const size_t t_NumOfSegments, SegmentsDirection t_Direction );

		// Returns the geometry segments that are slats made of
		std::shared_ptr< Viewer::CGeometry2D > geometry() const;

		double slatWidth() const;
		double slatSpacing() const;
		double slatTiltAngle() const;
		double curvatureRadius() const;
		size_t numberOfSegments() const;

	private:
		void buildSlat();

		double m_SlatWidth;
		double m_SlatSpacing;
		double m_SlatTiltAngle;
		double m_CurvatureRadius;
		size_t m_NumOfSlatSegments;
		SegmentsDirection m_Direction;

		std::shared_ptr< Viewer::CGeometry2D > m_Geometry;
	};

}

#endif
