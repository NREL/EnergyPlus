#include <stdexcept>

#include "BSDFLayerMaker.hpp"
#include "UniformDiffuseCell.hpp"
#include "DirectionalDiffuseCell.hpp"
#include "UniformDiffuseBSDFLayer.hpp"
#include "DirectionalDiffuseBSDFLayer.hpp"
#include "CellDescription.hpp"
#include "SpecularCellDescription.hpp"
#include "BaseCell.hpp"
#include "SpecularCell.hpp"
#include "SpecularBSDFLayer.hpp"
#include "VenetianCellDescription.hpp"
#include "VenetianCell.hpp"
#include "PerforatedCellDescription.hpp"
#include "PerforatedCell.hpp"
#include "WovenCellDescription.hpp"
#include "WovenCell.hpp"
#include "PerfectDiffuseCellDescription.hpp"

namespace SingleLayerOptics {

	CBSDFLayerMaker::CBSDFLayerMaker( const std::shared_ptr< CMaterial >& t_Material,
	                                  const std::shared_ptr< const CBSDFHemisphere >& t_BSDF, std::shared_ptr< ICellDescription > t_Description,
	                                  const DistributionMethod t_Method ) : m_Cell( nullptr ) {

		if ( t_Material == nullptr ) {
			throw std::runtime_error( "Material for BSDF layer must be defined." );
		}

		if ( t_BSDF == nullptr ) {
			throw std::runtime_error( "BSDF Definition for BSDF layer must be defined." );
		}

		// Specular BSDF layer is considered to be default. Default is used if t_Description is null pointer
		if ( t_Description == nullptr ) {
			t_Description = std::make_shared< CSpecularCellDescription >();
		}

		// Specular cell
		if ( std::dynamic_pointer_cast< CSpecularCellDescription >( t_Description ) != nullptr ) {
			std::shared_ptr< CSpecularCell > aCell = std::make_shared< CSpecularCell >( t_Material, t_Description );
			m_Cell = aCell;
			m_Layer = std::make_shared< CSpecularBSDFLayer >( aCell, t_BSDF );
		}

		if (std::dynamic_pointer_cast< CPerfectDiffuseCellDescription >(t_Description) != nullptr) {
			std::shared_ptr< CUniformDiffuseCell > aCell = std::make_shared< CUniformDiffuseCell >(t_Material, t_Description);
			m_Cell = aCell;
			m_Layer = std::make_shared< CUniformDiffuseBSDFLayer >(aCell, t_BSDF);
		}

		// Venetian cell
		if ( std::dynamic_pointer_cast< CVenetianCellDescription >( t_Description ) != nullptr ) {
			if ( t_Method == DistributionMethod::UniformDiffuse ) {
				std::shared_ptr< CUniformDiffuseCell > aCell = std::make_shared< CVenetianCell >( t_Material, t_Description );
				m_Cell = aCell;
				m_Layer = std::make_shared< CUniformDiffuseBSDFLayer >( aCell, t_BSDF );
			}
			else {
				std::shared_ptr< CDirectionalDiffuseCell > aCell = std::make_shared< CVenetianCell >( t_Material, t_Description );
				m_Cell = aCell;
				m_Layer = std::make_shared< CDirectionalDiffuseBSDFLayer >( aCell, t_BSDF );
			}
		}

		// Perforated cell
		if ( std::dynamic_pointer_cast< CPerforatedCellDescription >( t_Description ) != nullptr ) {
			// Perforated shades do not work with directional diffuse algorithm
			std::shared_ptr< CUniformDiffuseCell > aCell = std::make_shared< CPerforatedCell >( t_Material, t_Description );
			m_Cell = aCell;
			m_Layer = std::make_shared< CUniformDiffuseBSDFLayer >( aCell, t_BSDF );
		}

		// Woven cell
		if ( std::dynamic_pointer_cast< CWovenCellDescription >( t_Description ) != nullptr ) {
			// Woven shades do not work with directional diffuse algorithm
			std::shared_ptr< CUniformDiffuseCell > aCell = std::make_shared< CWovenCell >( t_Material, t_Description );
			m_Cell = aCell;
			m_Layer = std::make_shared< CUniformDiffuseBSDFLayer >( aCell, t_BSDF );
		}

	}

	std::shared_ptr< CBSDFLayer > CBSDFLayerMaker::getLayer() const {
		return m_Layer;
	}

	std::shared_ptr< CBaseCell > CBSDFLayerMaker::getCell() const {
		return m_Cell;
	}

}
