#ifndef PERFORATEDCELL_H
#define PERFORATEDCELL_H

#include <memory>

#include "UniformDiffuseCell.hpp"

namespace FenestrationCommon {

	enum class Side;

}

namespace SingleLayerOptics {

	class ICellDescription;

	class CPerforatedCell : public CUniformDiffuseCell {
	public:
		CPerforatedCell( const std::shared_ptr< CMaterial >& t_MaterialProperties,
		                 const std::shared_ptr< ICellDescription >& t_Cell );

	};

}

#endif
