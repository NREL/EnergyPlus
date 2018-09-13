#ifndef SPECULARCELLDESCRIPTION_H
#define SPECULARCELLDESCRIPTION_H

#include "CellDescription.hpp"

namespace FenestrationCommon {

	enum class Side;
	enum class MaterialType;

}

namespace SingleLayerOptics {

	class CSpecularCellDescription : public ICellDescription {
	public:
		CSpecularCellDescription();

		double T_dir_dir( const FenestrationCommon::Side t_Side, const CBeamDirection& t_Direction );
		double R_dir_dir( const FenestrationCommon::Side t_Side, const CBeamDirection& t_Direction );
		double Rspecular( const FenestrationCommon::Side t_Side,
		                  const CBeamDirection& t_Direction );

	};

}

#endif
