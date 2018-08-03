#ifndef PERFECTDIFFUSECELLDESRIPTION_H
#define PERFECTDIFFUSECELLDESRIPTION_H

#include "CellDescription.hpp"

namespace SingleLayerOptics {

	// Cell descritpion that needs to be used for perfect diffusers. Specular components are
	// set to zero
	class CPerfectDiffuseCellDescription : public ICellDescription {
	public:
		CPerfectDiffuseCellDescription();

		double T_dir_dir( const FenestrationCommon::Side t_Side, const CBeamDirection& t_Direction );
		double R_dir_dir( const FenestrationCommon::Side t_Side, const CBeamDirection& t_Direction );

	};

}

#endif
