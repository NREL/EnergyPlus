#include <cmath>
#include <stdexcept>

#include "WCECommon.hpp"
#include "BeamDirection.hpp"

using namespace std;
using namespace FenestrationCommon;

namespace SingleLayerOptics {

	CBeamDirection::CBeamDirection() : m_Theta( 0 ), m_Phi( 0 ) {
		updateProfileAngle( m_Theta, m_Phi );
	}

	CBeamDirection::CBeamDirection( const double t_Theta, const double t_Phi ) :
		m_Theta( t_Theta ), m_Phi( t_Phi ) {
		if ( t_Theta < 0 ) {
			throw runtime_error( "Theta angle cannot be less than zero degrees." );
		}
		if ( t_Theta > 90 ) {
			throw runtime_error( "Theta angle cannot be more than 90 degrees." );
		}

		updateProfileAngle( m_Theta, m_Phi );
	}

	double CBeamDirection::theta() const {
		return m_Theta;
	}

	double CBeamDirection::phi() const {
		return m_Phi;
	}

	double CBeamDirection::profileAngle() const {
		return m_ProfileAngle;
	}

	CBeamDirection& CBeamDirection::operator=( const CBeamDirection& t_SphericalPoint ) {
		m_Theta = t_SphericalPoint.m_Theta;
		m_Phi = t_SphericalPoint.m_Phi;
		m_ProfileAngle = t_SphericalPoint.m_ProfileAngle;
		return *this;
	}

	bool CBeamDirection::operator==( const CBeamDirection& t_SphericalPoint ) const {
		return ( m_Theta == t_SphericalPoint.m_Theta ) &&
			( m_Phi == t_SphericalPoint.m_Phi ) &&
			( m_ProfileAngle == t_SphericalPoint.m_ProfileAngle );
	}

	bool CBeamDirection::operator!=( const CBeamDirection& t_SphericalPoint ) const {
		return !( *this == t_SphericalPoint );
	}

	double CBeamDirection::distance( const double t_Theta, const double t_Phi ) const {
		return abs( m_Theta - t_Theta ) + abs( m_Phi - t_Phi );
	}

	double CBeamDirection::Altitude() const {
		double aTheta = radians( m_Theta );
		double aPhi = radians( m_Phi );
		return asin( sin( aTheta ) * -sin( aPhi ) );
	}

	double CBeamDirection::Azimuth() const {
		double aAltitude = Altitude();
		double aTheta = radians( m_Theta );
		double aPhi = radians( m_Phi );
		double aAzimuth = 0;
		if ( fabs( aTheta ) - fabs( aAltitude ) > 1e-8 ) {
			aAzimuth = -acos( cos( aTheta ) / cos( aAltitude ) );
		}
		if ( cos( aPhi ) < 0 ) {
			aAzimuth = -aAzimuth;
		}
		return aAzimuth;
	}

	void CBeamDirection::updateProfileAngle( const double t_Theta, const double t_Phi ) {
		m_ProfileAngle = -atan( sin( radians( t_Phi ) ) * tan( radians( t_Theta ) ) );
		m_ProfileAngle = degrees( m_ProfileAngle );
	}

}
