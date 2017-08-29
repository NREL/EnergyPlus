#include <cassert>

#include "SpecularCell.hpp"
#include "MaterialDescription.hpp"
#include "SpecularCellDescription.hpp"
#include "WCESpectralAveraging.hpp"
#include "BeamDirection.hpp"
#include "WCECommon.hpp"

using namespace std;
using namespace FenestrationCommon;
using namespace SpectralAveraging;

namespace SingleLayerOptics {

	CSpecularCell::CSpecularCell( const std::shared_ptr< CMaterial >& t_MaterialProperties,
	                              const std::shared_ptr< ICellDescription >& t_Cell ) : CBaseCell( t_MaterialProperties, t_Cell ) {

	}

	CSpecularCell::CSpecularCell( const std::shared_ptr< CMaterial >& t_MaterialProperties ) :
		CBaseCell( t_MaterialProperties, make_shared< CSpecularCellDescription >() ) {

	}

	double CSpecularCell::T_dir_dir( const Side t_Side, const CBeamDirection& t_Direction ) {
		// Specular glass transmittance is same for front and back sides
		return m_Material->getPropertyAtAngle( Property::T, t_Side, t_Direction.theta() );
	}

	double CSpecularCell::R_dir_dir( const Side t_Side, const CBeamDirection& t_Direction ) {
		return m_Material->getPropertyAtAngle( Property::R, t_Side, t_Direction.theta() );
	}

	// Transmittance of specular material for each of material range. In case of sample measured material, that are
	// properties at each wavelength. In case of some other material type, that will be properties for each of material
	// ranges
	vector< double > CSpecularCell::T_dir_dir_band( const Side t_Side,
	                                                const CBeamDirection& t_Direction ) {
		return m_Material->getBandPropertiesAtAngle( Property::T, t_Side, t_Direction.theta() );
	}

	// Reflectance of specular material over entire material range
	vector< double > CSpecularCell::R_dir_dir_band( const Side t_Side,
	                                                const CBeamDirection& t_Direction ) {
		return m_Material->getBandPropertiesAtAngle( Property::R, t_Side, t_Direction.theta() );
	}

	std::shared_ptr< CSpecularCellDescription > CSpecularCell::getCellAsSpecular() const {
		if ( dynamic_pointer_cast< CSpecularCellDescription >( m_CellDescription ) == NULL ) {
			assert("Incorrectly assigned cell description.");
		}

		std::shared_ptr< CSpecularCellDescription > aCell =
			dynamic_pointer_cast< CSpecularCellDescription >( m_CellDescription );

		return aCell;
	}

}
