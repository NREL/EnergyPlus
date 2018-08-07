#ifndef UniformDiffuseCell_H
#define UniformDiffuseCell_H

#include <memory>
#include <vector>

#include "BaseCell.hpp"

namespace FenestrationCommon {

	enum class Side;
	enum class Property;

}

namespace SingleLayerOptics {

	class ICellDescription;
	class CBeamDirection;
	class CMaterial;

	// Ray that hits uniformly diffusing cell material will distribute equally in every direction
	class CUniformDiffuseCell : public virtual CBaseCell {
	public:
		CUniformDiffuseCell( const std::shared_ptr< CMaterial >& t_MaterialProperties,
		                     const std::shared_ptr< ICellDescription >& t_Cell );

		// Direct to diffuse components
		virtual double T_dir_dif( const FenestrationCommon::Side t_Side, const CBeamDirection& t_Direction );

		virtual double R_dir_dif( const FenestrationCommon::Side t_Side, const CBeamDirection& t_Direction );

		// Property of the cell over the range
		virtual std::vector< double > T_dir_dif_band( const FenestrationCommon::Side t_Side,
		                                              const CBeamDirection& t_Direction );

		virtual std::vector< double > R_dir_dif_band( const FenestrationCommon::Side t_Side,
		                                              const CBeamDirection& t_Direction );

	private:
		double getMaterialProperty( const FenestrationCommon::Property t_Property,
		                            const FenestrationCommon::Side t_Side, const CBeamDirection& t_Direction );
		std::vector< double > getMaterialProperties( const FenestrationCommon::Property t_Property,
		                                             const FenestrationCommon::Side t_Side, const CBeamDirection& t_Direction );

	};

}

#endif
