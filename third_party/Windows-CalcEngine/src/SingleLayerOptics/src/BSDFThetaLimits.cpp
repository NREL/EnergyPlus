#include "BSDFThetaLimits.hpp"

namespace SingleLayerOptics {

	CThetaLimits::CThetaLimits( const std::vector< double >& t_ThetaAngles ) {
		if ( t_ThetaAngles.size() == 0 ) {
			throw std::runtime_error( "Error in definition of theta angles. Cannot form theta definitions." );
		}
		m_ThetaLimits = std::make_shared< std::vector< double > >();
		createLimits( t_ThetaAngles );
	}

	std::shared_ptr< std::vector< double > > CThetaLimits::getThetaLimits() const {
		return m_ThetaLimits;
	}

	void CThetaLimits::createLimits( const std::vector< double >& t_ThetaAngles ) {
		std::vector< double >::const_reverse_iterator it;
		double previousAngle = 90;
		m_ThetaLimits->push_back( previousAngle );

		for ( it = t_ThetaAngles.rbegin(); it < t_ThetaAngles.rend(); ++it ) {
			double currentAngle = ( *it );
			double delta = 2 * ( previousAngle - currentAngle );
			double limit = previousAngle - delta;
			if ( limit < 0 ) {
				limit = 0;
			}
			m_ThetaLimits->insert( m_ThetaLimits->begin(), limit );
			previousAngle = limit;
		}
	}
}
