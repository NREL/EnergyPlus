#include <vector>

#include "Hemispherical2DIntegrator.hpp"
#include "Series.hpp"
#include "IntegratorStrategy.hpp"
#include "MathFunctions.hpp"



namespace FenestrationCommon {

	// Performs hemispherical 2D integration
	CHemispherical2DIntegrator::CHemispherical2DIntegrator( CSeries const& t_Series,
	                                                        IntegrationType const t_IntegrationType ) {
		CSeries aResultValues = CSeries();
		for ( auto const& ser : t_Series ) {
			auto angle = radians( ser->x() );
			auto value = ser->value();
			auto sinCos = sin( angle ) * cos( angle );
			aResultValues.addProperty( angle, value * sinCos );
		}

		aResultValues.sort();

		auto integrated = aResultValues.integrate( t_IntegrationType );
		m_Value = 2 * integrated->sum();
	}

	double CHemispherical2DIntegrator::value() const {
		return m_Value;
	}

}
