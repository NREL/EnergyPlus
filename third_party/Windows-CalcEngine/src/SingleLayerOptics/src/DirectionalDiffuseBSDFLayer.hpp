#ifndef DIRECTIONALDIFFUSEBSDFLAYER_H
#define DIRECTIONALDIFFUSEBSDFLAYER_H

#include <memory>

#include "BSDFLayer.hpp"

namespace SingleLayerOptics {

	class CDirectionalDiffuseCell;

	// All outgoing directions are calculated
	class CDirectionalDiffuseBSDFLayer : public CBSDFLayer {
	public:
		CDirectionalDiffuseBSDFLayer( const std::shared_ptr< CDirectionalDiffuseCell >& t_Cell,
		                              const std::shared_ptr< const CBSDFHemisphere >& t_Hemisphere );

	protected:
		std::shared_ptr< CDirectionalDiffuseCell > cellAsDirectionalDiffuse() const;
		void calcDiffuseDistribution( const FenestrationCommon::Side aSide,
		                              const CBeamDirection& t_Direction,
		                              const size_t t_DirectionIndex );
		void calcDiffuseDistribution_wv( const FenestrationCommon::Side aSide,
		                                 const CBeamDirection& t_Direction,
		                                 const size_t t_DirectionIndex );

	};

}

#endif
