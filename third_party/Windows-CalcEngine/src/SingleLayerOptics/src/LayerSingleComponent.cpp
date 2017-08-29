#include <stdexcept>
#include <cassert>

#include "LayerSingleComponent.hpp"
#include "WCECommon.hpp"
#include "MaterialDescription.hpp"
#include "OpticalSurface.hpp"

using namespace std;
using namespace FenestrationCommon;

namespace SingleLayerOptics {

	/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// CLayerSingleComponent
	/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	CLayerSingleComponent::CLayerSingleComponent( const double t_Tf, const double t_Rf, const double t_Tb,
	                                              const double t_Rb ) {
		m_Surface[ Side::Front ] = make_shared< CSurface >( t_Tf, t_Rf );
		m_Surface[ Side::Back ] = make_shared< CSurface >( t_Tb, t_Rb );
	}

	double CLayerSingleComponent::getProperty( const Property t_Property, const Side t_Side ) const {
		std::shared_ptr< CSurface > aSurface = getSurface( t_Side );
		assert( aSurface != nullptr );
		return aSurface->getProperty( t_Property );
	}

	std::shared_ptr< CSurface > CLayerSingleComponent::getSurface( const Side t_Side ) const {
		return m_Surface.at( t_Side );
	}

}
