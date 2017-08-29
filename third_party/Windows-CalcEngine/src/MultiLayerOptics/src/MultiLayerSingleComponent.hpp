#ifndef MULTILAYERSINGLECOMPONENT_H
#define MULTILAYERSINGLECOMPONENT_H

#include <memory>

#include "WCECommon.hpp"

namespace MultiLayerOptics {

	class CEquivalentLayerSingleComponent;
	class CInterRefSingleComponent;

	// Class to calculate multilayer optical properties for single component (direct or diffuse)
	class CMultiLayerSingleComponent {
	public:
		CMultiLayerSingleComponent( const double t_Tf, const double t_Rf, const double t_Tb, const double t_Rb );

		// Adding layer to front or back side of composition
		void addLayer( const double t_Tf, const double t_Rf, const double t_Tb, const double t_Rb,
		               FenestrationCommon::Side t_Side = FenestrationCommon::Side::Back );

		// Get oprical properties of equivalent layer
		double getProperty( const FenestrationCommon::Property t_Property,
		                    const FenestrationCommon::Side t_Side ) const;

		double getLayerAbsorptance( const size_t Index, const FenestrationCommon::Side t_Side );

	private:
		std::shared_ptr< CInterRefSingleComponent > m_Inter;
		std::shared_ptr< CEquivalentLayerSingleComponent > m_Equivalent;

	};

}

#endif
