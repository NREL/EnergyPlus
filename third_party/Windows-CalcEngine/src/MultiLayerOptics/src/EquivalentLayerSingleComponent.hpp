#ifndef EQUIVALENTLAYERSINGLECOMPONENT_H
#define EQUIVALENTLAYERSINGLECOMPONENT_H

#include <memory>

#include "WCECommon.hpp"

namespace SingleLayerOptics {

	class CLayerSingleComponent;

}

namespace MultiLayerOptics {

	// Class to calculate equivalent layer transmittance and reflectances only.
	class CEquivalentLayerSingleComponent {
	public:
		CEquivalentLayerSingleComponent( const double t_Tf, const double t_Rf, const double t_Tb, const double t_Rb );
		CEquivalentLayerSingleComponent( const SingleLayerOptics::CLayerSingleComponent& t_Layer );

		// Adding layer to front or back side of the IGU composition
		void addLayer( const double t_Tf, const double t_Rf, const double t_Tb, const double t_Rb,
		               const FenestrationCommon::Side t_Side = FenestrationCommon::Side::Back );

		void addLayer( const SingleLayerOptics::CLayerSingleComponent& t_Layer,
		               const FenestrationCommon::Side t_Side = FenestrationCommon::Side::Back );

		double getProperty( const FenestrationCommon::Property t_Property,
		                    const FenestrationCommon::Side t_Side ) const;

		std::shared_ptr< SingleLayerOptics::CLayerSingleComponent > getLayer() const;

	private:
		double interreflectance( const SingleLayerOptics::CLayerSingleComponent& t_Layer1,
		                         const SingleLayerOptics::CLayerSingleComponent& t_Layer2 ) const;

		double T( const SingleLayerOptics::CLayerSingleComponent& t_Layer1,
		          const SingleLayerOptics::CLayerSingleComponent& t_Layer2,
		          FenestrationCommon::Side t_Side ) const;

		double R( const SingleLayerOptics::CLayerSingleComponent& t_Layer1,
		          const SingleLayerOptics::CLayerSingleComponent& t_Layer2,
		          FenestrationCommon::Side t_Side ) const;

		std::shared_ptr< SingleLayerOptics::CLayerSingleComponent > m_EquivalentLayer;

	};

}

#endif
