
#include <cmath>

#include "PolarPoint2D.hpp"
#include "WCECommon.hpp"

using namespace FenestrationCommon;

namespace Viewer {

	CPolarPoint2D::CPolarPoint2D( double const t_Theta, double const t_Radius ) :
		CPoint2D( 0, 0 ), m_Theta( t_Theta ), m_Radius( t_Radius ) {
		auto aTheta = radians( m_Theta );
		m_x = m_Radius * std::cos( aTheta );
		m_y = m_Radius * std::sin( aTheta );
	}

	double CPolarPoint2D::theta() const {
		return m_Theta;
	}

	double CPolarPoint2D::radius() const {
		return m_Radius;
	}

	void CPolarPoint2D::setCartesian( double const x, double const y ) {
		using ConstantsData::WCE_PI;

		m_x = x;
		m_y = y;

		if ( x != 0 ) {
			m_Theta = std::atan( y / x );
		}
		else if ( x == 0 && y > 0 ) {
			m_Theta = WCE_PI / 2;
		}
		else if ( x == 0 && y < 0 ) {
			m_Theta = 3 * WCE_PI / 2;
		}
		else {
			m_Theta = 0;
		}

		if ( std::sin( m_Theta ) != 0 ) {
			m_Radius = y / std::sin( m_Theta );
		}
		else if ( std::cos( m_Theta ) != 0 ) {
			m_Radius = x / std::cos( m_Theta );
		}
		else {
			m_Radius = 0;
		}

		// always store angles in degrees
		m_Theta = degrees( m_Theta );

	}

}
