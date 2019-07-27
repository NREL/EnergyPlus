#include "PerforatedCell.hpp"
#include "CellDescription.hpp"
#include "MaterialDescription.hpp"
#include "WCECommon.hpp"

using namespace FenestrationCommon;

namespace SingleLayerOptics {

	////////////////////////////////////////////////////////////////////////////////////////////
	//  CPerforatedCell
	////////////////////////////////////////////////////////////////////////////////////////////
	CPerforatedCell::CPerforatedCell( const std::shared_ptr< CMaterial >& t_MaterialProperties,
	                                  const std::shared_ptr< ICellDescription >& t_Cell ) :
		CBaseCell( t_MaterialProperties, t_Cell ), CUniformDiffuseCell( t_MaterialProperties, t_Cell ) {

	}

}
