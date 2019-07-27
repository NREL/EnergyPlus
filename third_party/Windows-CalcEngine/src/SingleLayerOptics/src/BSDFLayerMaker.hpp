#ifndef BSDFLAYERMAKER_H
#define BSDFLAYERMAKER_H

#include <memory>

namespace SingleLayerOptics {

	enum class DistributionMethod { UniformDiffuse, DirectionalDiffuse };

	class ICellDescription;
	class CMaterial;
	class CBSDFHemisphere;
	class CBSDFLayer;
	class CBaseCell;

	// Class to simplify interface for BSDF layer creation
	class CBSDFLayerMaker {
	public:
		CBSDFLayerMaker( const std::shared_ptr< CMaterial >& t_Material,
		                 const std::shared_ptr< const CBSDFHemisphere >& t_BSDF,
		                 std::shared_ptr< ICellDescription > t_Description = nullptr,
		                 const DistributionMethod t_Method = DistributionMethod::UniformDiffuse );

		std::shared_ptr< CBSDFLayer > getLayer() const;

		std::shared_ptr< CBaseCell > getCell() const;

	private:
		std::shared_ptr< CBSDFLayer > m_Layer;
		std::shared_ptr< CBaseCell > m_Cell;
	};

}

#endif
