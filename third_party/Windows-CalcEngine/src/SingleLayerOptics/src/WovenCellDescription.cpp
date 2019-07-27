
#include <cmath>
#include <cassert>
#include <stdexcept>

#include "WovenCellDescription.hpp"
#include "BeamDirection.hpp"
#include "WCECommon.hpp"

using namespace FenestrationCommon;

namespace SingleLayerOptics {

	CWovenCellDescription::CWovenCellDescription( const double t_Diameter, const double t_Spacing ) :
		ICellDescription(), m_Diameter( t_Diameter ), m_Spacing( t_Spacing ) {
		if ( m_Diameter <= 0 ) {
			throw std::runtime_error( "Woven shade diameter must be greater than zero." );
		}
		if ( m_Spacing <= 0 ) {
			throw std::runtime_error( "Woven shade threads spacing must be greater than zero." );
		}
	}

	double CWovenCellDescription::diameter() const {
		return m_Diameter;
	}

	double CWovenCellDescription::spacing() const {
		return m_Spacing;
	}

	double CWovenCellDescription::gamma() const {
		assert( m_Spacing > 0 );
		return m_Diameter / m_Spacing;
	}

	double CWovenCellDescription::cutOffAngle() const {
		return acos( gamma() );
	}

	double CWovenCellDescription::T_dir_dir( const Side, const CBeamDirection& t_Direction ) {
		return Tx( t_Direction ) * Ty( t_Direction );
	}

	double CWovenCellDescription::R_dir_dir( const Side, const CBeamDirection& ) {
		return 0;
	}

	double CWovenCellDescription::Tx( const CBeamDirection& t_Direction ) {
		using ConstantsData::PI;

		double aTx = 0;
		double cutOffAngle = this->cutOffAngle();
		double aAzimuth = t_Direction.Azimuth();
		if ( aAzimuth > PI / 2 ) {
			aAzimuth = PI - aAzimuth;
		}
		if ( aAzimuth < -PI / 2 ) {
			aAzimuth = -PI - aAzimuth;
		}
		aAzimuth = std::abs( aAzimuth );
		if ( aAzimuth < cutOffAngle ) {
			aTx = 1 - this->gamma() / cos( aAzimuth );
		}
		return aTx;
	}

	double CWovenCellDescription::Ty( const CBeamDirection& t_Direction ) {
		double aTy = 0;
		double cutOffAngle = this->cutOffAngle();
		double aAltitude = t_Direction.Altitude();
		double aPrim = std::abs( atan( tan( aAltitude ) / cos( aAltitude ) ) );
		if ( aPrim < cutOffAngle ) {
			aTy = 1 - this->gamma() / cos( aPrim );
		}

		return aTy;
	}

}
