#ifndef WOVENCELL_H
#define WOVENCELL_H

#include <memory>

#include "UniformDiffuseCell.hpp"

namespace SingleLayerOptics {

	class CWovenCellDescription;
	class ICellDescription;
	class CBeamDirection;

	class CWovenBase : public CUniformDiffuseCell {
	public:
		CWovenBase( const std::shared_ptr< CMaterial >& t_MaterialProperties,
		            const std::shared_ptr< ICellDescription >& t_Cell );

	protected:
		std::shared_ptr< CWovenCellDescription > getCellAsWoven() const;
	};

	class CWovenCell : public CWovenBase {
	public:
		CWovenCell( const std::shared_ptr< CMaterial >& t_MaterialProperties,
		            const std::shared_ptr< ICellDescription >& t_Cell );

		double T_dir_dir( const FenestrationCommon::Side t_Side, const CBeamDirection& t_Direction );
		double T_dir_dif( const FenestrationCommon::Side t_Side, const CBeamDirection& t_Direction );
		double R_dir_dif( const FenestrationCommon::Side t_Side, const CBeamDirection& t_Direction );

		std::vector< double > T_dir_dir_band( const FenestrationCommon::Side t_Side,
		                                      const CBeamDirection& t_Direction );
		std::vector< double > T_dir_dif_band( const FenestrationCommon::Side t_Side,
		                                      const CBeamDirection& t_Direction );
		std::vector< double > R_dir_dif_band( const FenestrationCommon::Side t_Side,
		                                      const CBeamDirection& t_Direction );

	private:
		double Tscatter_single( const FenestrationCommon::Side t_Side,
		                        const CBeamDirection& t_Direction );
		std::vector< double > Tscatter_range( const FenestrationCommon::Side t_Side,
		                                      const CBeamDirection& t_Direction );

		// Calculates scattered part of reflection from woven
		double Tscatter( const CBeamDirection& t_Direction, const double Rmat );

	};

}

#endif
