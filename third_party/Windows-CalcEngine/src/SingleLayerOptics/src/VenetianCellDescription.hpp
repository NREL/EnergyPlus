#ifndef VENETIANCELLDESCRIPTION_H
#define VENETIANCELLDESCRIPTION_H

#include <memory>
#include <vector>

#include "CellDescription.hpp"

namespace Viewer {

	class CGeometry2D;
	struct BeamViewFactor;
	class CGeometry2DBeam;

}

namespace FenestrationCommon {

	class CSquareMatrix;
	enum class Side;

}

namespace SingleLayerOptics {

	class CVenetianSlat;

	class CVenetianCellDescription : public ICellDescription {
	public:
		CVenetianCellDescription( const double t_SlatWidth, const double t_SlatSpacing, const double t_SlatTiltAngle,
		                          const double t_CurvatureRadius, const size_t t_NumOfSlatSegments );

		// Makes exact copy of cell description
		std::shared_ptr< CVenetianCellDescription > makeBackwardCell() const;
		size_t numberOfSegments() const;
		double segmentLength( const size_t Index ) const;

		// View factors of enclosure slats
		std::shared_ptr< FenestrationCommon::CSquareMatrix > viewFactors();

		// view factor of the beam entering the cell with profile angle
		std::shared_ptr< std::vector< Viewer::BeamViewFactor > >
		beamViewFactors( const double t_ProfileAngle, const FenestrationCommon::Side t_Side );

		// Direct to direct component of the ray
		double T_dir_dir( const FenestrationCommon::Side t_Side, const CBeamDirection& t_Direction );
		double R_dir_dir( const FenestrationCommon::Side t_Side, const CBeamDirection& t_Direction );

	private:
		// Top and bottom slats of venetian cell
		std::shared_ptr< CVenetianSlat > m_Top;
		std::shared_ptr< CVenetianSlat > m_Bottom;

		// Complete enclosure from venetian cell
		std::shared_ptr< Viewer::CGeometry2D > m_Geometry;

		// Geometry to handle direct to direct beam component
		std::shared_ptr< Viewer::CGeometry2DBeam > m_BeamGeometry;
	};

}

#endif
