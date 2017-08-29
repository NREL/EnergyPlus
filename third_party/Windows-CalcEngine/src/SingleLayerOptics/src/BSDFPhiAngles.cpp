#include "BSDFPhiAngles.hpp"

using namespace std;

namespace SingleLayerOptics {

	CBSDFPhiAngles::CBSDFPhiAngles( const size_t t_NumOfPhis ) :
		m_PhiAngles( make_shared< std::vector< double > >() ) {
		createPhis( t_NumOfPhis );
	}

	std::shared_ptr< std::vector< double > > CBSDFPhiAngles::phiAngles() const {
		return m_PhiAngles;
	}

	void CBSDFPhiAngles::createPhis( const size_t t_NumOfPhis ) {
		const double phiDelta = 360.0 / t_NumOfPhis;
		for ( size_t i = 0; i < t_NumOfPhis; ++i ) {
			m_PhiAngles->push_back( i * phiDelta );
		}
	}

}
