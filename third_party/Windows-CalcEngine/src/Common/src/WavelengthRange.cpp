#include <cassert>

#include "WavelengthRange.hpp"
#include "WCECommon.hpp"



namespace FenestrationCommon {

	/////////////////////////////////////////////////////////////////////////////////////////////
	////   CWavelengthRange
	/////////////////////////////////////////////////////////////////////////////////////////////

	CWavelengthRange::CWavelengthRange( const WavelengthRange t_Range ) {
		setWavelengthRange( t_Range );
	}

	double CWavelengthRange::minLambda() const {
		return m_MinLambda;
	}

	double CWavelengthRange::maxLambda() const {
		return m_MaxLambda;
	}

	void CWavelengthRange::setWavelengthRange( const WavelengthRange t_Range ) {
		switch ( t_Range ) {
		case WavelengthRange::IR:
			m_MinLambda = 5.0;
			m_MaxLambda = 100.0;
			break;
		case WavelengthRange::Solar:
			m_MinLambda = 0.3;
			m_MaxLambda = 2.5;
			break;
		case WavelengthRange::Visible:
			m_MinLambda = 0.38;
			m_MaxLambda = 0.78;
			break;
		default:
			assert("Incorrect call for wavelength range creation.");
			break;
		}

	}

}
