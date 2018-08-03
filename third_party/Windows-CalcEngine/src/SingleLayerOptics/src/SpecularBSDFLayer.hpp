#ifndef SPECULARBSDFLAYER_H
#define SPECULARBSDFLAYER_H

#include <memory>
#include <vector>

#include "BSDFLayer.hpp"

namespace SingleLayerOptics {

	class CSpecularCell;

	// BSDF creation for specular layers.
	class CSpecularBSDFLayer : public CBSDFLayer {
	public:
		CSpecularBSDFLayer( const std::shared_ptr< CSpecularCell >& t_Cell,
		                    const std::shared_ptr< const CBSDFHemisphere >& t_Hemisphere );

	protected:
		std::shared_ptr< CSpecularCell > cellAsSpecular() const;
		void calcDiffuseDistribution( const FenestrationCommon::Side aSide,
		                              const CBeamDirection& t_Direction,
		                              const size_t t_DirectionIndex );
		void calcDiffuseDistribution_wv( const FenestrationCommon::Side aSide,
		                                 const CBeamDirection& t_Direction,
		                                 const size_t t_DirectionIndex );


	};

}

#endif
