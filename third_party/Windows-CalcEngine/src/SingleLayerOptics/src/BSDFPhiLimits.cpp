#include "BSDFPhiLimits.hpp"
#include "BSDFPhiAngles.hpp"

using namespace std;

namespace SingleLayerOptics {

	CPhiLimits::CPhiLimits( const size_t t_NumOfPhis ) {
		if ( t_NumOfPhis == 0 ) {
			throw runtime_error( "Number of phi angles for BSDF definition must be greater than zero." );
		}
		m_PhiLimits = make_shared< std::vector< double > >();

		CBSDFPhiAngles aPhiAngles( t_NumOfPhis );

		createLimits( *aPhiAngles.phiAngles() );
	}

	std::shared_ptr< std::vector< double > > CPhiLimits::getPhiLimits() const {
		return m_PhiLimits;
	}

	void CPhiLimits::createLimits( const std::vector< double >& t_PhiAngles ) {
		double deltaPhi = 360.0 / t_PhiAngles.size();
		double currentLimit = -deltaPhi / 2;
		if ( t_PhiAngles.size() == 1 ) {
			currentLimit = 0;
		}

		for ( size_t i = 0; i <= t_PhiAngles.size(); ++i ) {
			m_PhiLimits->push_back( currentLimit );
			currentLimit = currentLimit + deltaPhi;
		}
	}
}
