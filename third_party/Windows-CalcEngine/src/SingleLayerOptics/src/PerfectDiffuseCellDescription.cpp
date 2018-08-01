#include "PerfectDiffuseCellDescription.hpp"

using namespace FenestrationCommon;

namespace SingleLayerOptics {

	CPerfectDiffuseCellDescription::CPerfectDiffuseCellDescription() : ICellDescription() {

	}

	double CPerfectDiffuseCellDescription::T_dir_dir( const Side, const CBeamDirection& ) {
		return 0;
	}

	double CPerfectDiffuseCellDescription::R_dir_dir( const Side, const CBeamDirection& ) {
		return 0;
	}

}
